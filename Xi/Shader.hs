{-# LANGUAGE OverloadedStrings #-}

module Xi.Shader
  ( makeProgram
  , makeProgramFromFiles
  , makeProgramFromSources
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Char8 as BC

import Xi.Imports
import Xi.Debug


compileShaderSource :: BS.ByteString -> GLenum -> IO GLuint
compileShaderSource source shaderType = do
  shader <- glCreateShader shaderType
  BS.unsafeUseAsCString (source <> "\0") $ \sourcePtr -> do
    with sourcePtr $ \sourcePtrRef -> do
      glShaderSource shader 1 sourcePtrRef nullPtr

  glCompileShader shader 

  status <- alloca $ \statusPtr -> do
    glGetShaderiv shader gl_COMPILE_STATUS statusPtr
    peek statusPtr

  when (status == 0) $ do
    infoLogLen <- alloca $ \infoLogLenPtr -> do
      glGetShaderiv shader gl_INFO_LOG_LENGTH infoLogLenPtr
      peek infoLogLenPtr
    infoLog <- allocaArray (fromIntegral $ infoLogLen + 1) $ \ptr -> do
      glGetShaderInfoLog shader infoLogLen nullPtr ptr
      BC.unpack <$> BS.unsafePackCString (castPtr ptr)
    glDeleteShader shader
    ioError . userError $ "error: compileShaderFile - " ++ infoLog

  return shader


compileShaderFile :: FilePath -> GLenum -> IO GLuint
compileShaderFile filePath shaderType = do
  source <- BS.readFile filePath
  compileShaderSource source shaderType


makeProgramFromSources :: BS.ByteString -> BS.ByteString -> IO GLuint
makeProgramFromSources vsrc fsrc = do
  vsh <- compileShaderSource vsrc gl_VERTEX_SHADER
  fsh <- compileShaderSource fsrc gl_FRAGMENT_SHADER
  makeProgram vsh fsh


makeProgramFromFiles :: FilePath -> FilePath -> IO GLuint
makeProgramFromFiles  filePathVsh filePathFsh = do
  vsh <- compileShaderFile filePathVsh gl_VERTEX_SHADER
  fsh <- compileShaderFile filePathFsh gl_FRAGMENT_SHADER
  makeProgram vsh fsh


makeProgram :: GLuint -> GLuint -> IO GLuint
makeProgram vsh fsh = do
  program <- glCreateProgram
  glAttachShader program vsh
  glAttachShader program fsh
  glLinkProgram program

  status <- alloca $ \statusPtr -> do
    glGetProgramiv program gl_LINK_STATUS statusPtr
    peek statusPtr

  when (status == 0) $ do
    infoLogLen <- alloca $ \infoLogLenPtr -> do
      glGetProgramiv program gl_INFO_LOG_LENGTH infoLogLenPtr
      peek infoLogLenPtr
    infoLog <- allocaArray (fromIntegral infoLogLen) $ \ptr -> do
      glGetProgramInfoLog program infoLogLen nullPtr ptr
      BC.unpack <$> BS.unsafePackCString (castPtr ptr)
    glDeleteProgram program
    ioError . userError $ "error: makeProgram - " ++ infoLog

  return program


makeVertexBufferObject :: GLenum -> GLenum -> Ptr a -> GLsizeiptr -> IO GLuint
makeVertexBufferObject bufferTarget bufferUsage datPtr datSize = do
  vbo <- alloca $ \vboPtr -> do
    glGenBuffers 1 vboPtr
    peek vboPtr
  glBindBuffer bufferTarget vbo
  glBufferData bufferTarget datSize datPtr bufferUsage
  return vbo

