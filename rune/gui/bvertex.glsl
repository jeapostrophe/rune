#version 130
in vec2 @|in_Position|;
in vec2 @|in_Dimension|;
in vec2 @|in_TexDimension|;
in vec2 @|in_Viewport|;
in ivec2 @|in_Vertex|;
in vec2 @|in_Offset|;

out vec2 @|TexCoord|;

@GLSL-Library

void main ( void )
{
  // xxx y goes down when visual goes up :(
  mat4 ZeMatrix =
    glTranslate( @|in_Position|.x, @|in_Position|.y, 0.0)
    * glOrtho(0.0, @|in_Viewport|.x,
              0.0, @|in_Viewport|.y,
              1.0, -1.0);
  float px = (@|in_Vertex|.x * @|in_Dimension|.x);
  float py = (@|in_Vertex|.y * @|in_Dimension|.y);
  gl_Position = vec4( px, py, 0.0, 1.0) * ZeMatrix;

  @|TexCoord| = vec2( (@|in_Offset|.x + (@|in_Vertex|.x * @|in_Dimension|.x)) /
                      @|in_TexDimension|.x,
                      (@|in_Offset|.y + (@|in_Vertex|.y * @|in_Dimension|.y)) /
                      @|in_TexDimension|.y);
}
