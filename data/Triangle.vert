
varying vec2 coor;

void main()
{
	coor.x = gl_Vertex.x;
	coor.y = gl_Vertex.y;
	gl_Position = gl_Vertex;
}
