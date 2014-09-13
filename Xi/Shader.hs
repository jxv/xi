{-# LANGUAGE OverloadedStrings #-}

module Xi.Shader
  ( makeProgram
  ) where

------------------------------------------------------------------------------------------

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as BC

------------------------------------------------------------------------------------------

import Xi.Imports
import Xi.Debug

------------------------------------------------------------------------------------------

compileShaderFile :: FilePath -> GLenum -> IO GLuint
compileShaderFile filePath shaderType = do
  -- Read shader source code
  source <- B.readFile filePath
  -- Generate shader's ident then set the source code
  shader <- glCreateShader shaderType
  B.unsafeUseAsCString (source <> "\0") $ \sourcePtr -> do
    with sourcePtr $ \sourcePtrRef -> do
      glShaderSource shader 1 sourcePtrRef nullPtr
  --
  glCompileShader shader 
  --
  status <- alloca $ \statusPtr -> do
    glGetShaderiv shader gl_COMPILE_STATUS statusPtr
    peek statusPtr
  --
  --
  when (status == 0) $ do
    infoLogLen <- alloca $ \infoLogLenPtr -> do
      glGetShaderiv shader gl_INFO_LOG_LENGTH infoLogLenPtr
      peek infoLogLenPtr
    infoLog <- allocaArray (fromIntegral $ infoLogLen + 1) $ \ptr -> do
      glGetShaderInfoLog shader infoLogLen nullPtr ptr
      BC.unpack <$> B.unsafePackCString (castPtr ptr)
    glDeleteShader shader
    ioError . userError $ "error: compileShaderFile '" ++ filePath ++ "' - " ++ infoLog
  --
  return shader

makeProgram :: FilePath -> FilePath -> IO GLuint
makeProgram  filePathVsh filePathFsh = do
  vsh <- compileShaderFile filePathVsh gl_VERTEX_SHADER
  fsh <- compileShaderFile filePathFsh gl_FRAGMENT_SHADER
  --
  program <- glCreateProgram
  glAttachShader program vsh
  glAttachShader program fsh
  glLinkProgram program
  --
  status <- alloca $ \statusPtr -> do
    glGetProgramiv program gl_LINK_STATUS statusPtr
    peek statusPtr
  --
  when (status == 0) $ do
    infoLogLen <- alloca $ \infoLogLenPtr -> do
      glGetProgramiv program gl_INFO_LOG_LENGTH infoLogLenPtr
      peek infoLogLenPtr
    infoLog <- allocaArray (fromIntegral infoLogLen) $ \ptr -> do
      glGetProgramInfoLog program infoLogLen nullPtr ptr
      BC.unpack <$> B.unsafePackCString (castPtr ptr)
    glDeleteProgram program
    ioError . userError $ "error: makeProgram - " ++ infoLog
  --
  return program

------------------------------------------------------------------------------------------

makeVertexBufferObject :: GLenum -> GLenum -> Ptr a -> GLsizeiptr -> IO GLuint
makeVertexBufferObject bufferTarget bufferUsage datPtr datSize = do
  vbo <- alloca $ \vboPtr -> do
    glGenBuffers 1 vboPtr
    peek vboPtr
  glBindBuffer bufferTarget vbo
  glBufferData bufferTarget datSize datPtr bufferUsage
  return vbo

