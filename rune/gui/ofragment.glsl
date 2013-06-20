#version 130
// xxx add to connected
in vec4 Vertex;
in vec4 @|Color|;

out vec4 out_Color;

float BORDER = 1;

void main ( void )
{
  if ((Vertex.x < BORDER || (Vertex.x - Vertex[2]) > -BORDER)
      || (Vertex.y < BORDER || (Vertex.y - Vertex[3]) > -BORDER)) {
    out_Color = @|Color|;
  } else {
    discard;
  }
}
