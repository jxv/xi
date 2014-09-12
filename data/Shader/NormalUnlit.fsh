
varying vec2 coor;

void main ()
{
	gl_FragColor = vec4(0.7, coor.x, coor.y, 1.0);
}
