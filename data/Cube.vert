
uniform mat4 u_modelViewProjMat;

attribute vec4 a_position;

varying vec4 v_position;

void main()
{
  gl_Position = u_modelViewProjMat * a_position;
  v_position = a_position;
}

