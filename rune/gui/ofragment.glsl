#version 130
uniform sampler2D @|ColorTex|;

// xxx add to connected
in vec4 Vertex;
in float @|Color|;

out vec4 out_Color;

float BORDER = 1;

void main ( void )
{
  if ((Vertex.x < BORDER || (Vertex.x - Vertex[2]) > -BORDER)
      || (Vertex.y < BORDER || (Vertex.y - Vertex[3]) > -BORDER)) {
    // xxx move to vertex
    ivec2 ColorCoord = ivec2(@|Color|, 0);
    out_Color = texelFetch(@|ColorTex|, ColorCoord, 0);
  } else {
    discard;
  }
}
