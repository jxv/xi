
varying vec2 coor;

uniform mat4 modelViewProjectionMat;

void main()
{
	coor.x = gl_Vertex.x;
	coor.y = gl_Vertex.y;
	gl_Position = gl_Vertex;
}
