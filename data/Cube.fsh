
varying vec4 v_position;

void main()
{
  vec4 color = abs(v_position * 2.0);
  color.w = 1.0;
  gl_FragColor = color;
}

