#version 130
uniform sampler2D @|ColorTex|;
uniform vec2 in_Viewport;

in vec2 @|in_Position|;
in vec2 @|in_Dimension|;
in ivec2 @|in_Vertex|;
in uint @|in_Color|;

out vec4 Vertex;
out vec4 @|Color|;

@GLSL-Library

void main ( void )
{
  mat4 ZeMatrix =
    glTranslate( @|in_Position|.x, in_Viewport.y - @|in_Position|.y, 0.0)
    * glOrtho(0.0, in_Viewport.x,
              0.0, in_Viewport.y, 
              1.0, -1.0);
  float px = (@|in_Vertex|.x * @|in_Dimension|.x);
  float py = (@|in_Vertex|.y * @|in_Dimension|.y);
  gl_Position = vec4( px, py, 0.0, 1.0) * ZeMatrix;

  ivec2 ColorCoord = ivec2(@|in_Color|, 0);
  @|Color| = texelFetch(@|ColorTex|, ColorCoord, 0);

  Vertex = vec4(px, py, @|in_Dimension|.x, @|in_Dimension|.y);
}
