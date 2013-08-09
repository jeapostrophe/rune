#version 130
uniform float @|CharSide|;
uniform float @|CharWidth|;
uniform float @|CharHeight|;
uniform sampler2D @|ColorTex|;
uniform vec2 @|in_Viewport|;

in uvec2 @|in_Position_rc|;
in ivec2 @|in_Vertex|;
in uint @|in_Char|;
in uint @|in_Color|;

out vec4 @|FColor|;
out vec4 @|BColor|;
out vec2 @|TexCoord|;

@GLSL-Library

void main ( void )
{
  vec2 in_Position = vec2( @|in_Position_rc|.y * @|CharWidth|,
                           @|in_Position_rc|.x * @|CharHeight| );

  mat4 ZeMatrix =
    glTranslate( in_Position.x, in_Position.y, 0.0)
    * glOrtho(0.0, @|in_Viewport|.x,
              0.0, @|in_Viewport|.y,
              1.0, -1.0);
  float px = (@|in_Vertex|.x * @|CharWidth|);
  float py = (@|in_Vertex|.y * @|CharHeight|);
  gl_Position = vec4( px, py, 0.0, 1.0) * ZeMatrix;

  float ci = float(@|in_Char|);
  float cy = floor(ci / @|CharSide|);
  float cx = ci - cy * @|CharSide|;
  @|TexCoord| = vec2( ((cx * @|CharWidth|) + px), ((cy * @|CharHeight|) + py) );

  uint FCR = (@|in_Color|) & uint(0x0F);
  uint BCR = ((@|in_Color|) >> 4) & uint(0x0F);
  @|FColor| = texelFetch(@|ColorTex|, ivec2(FCR, 0), 0);
  @|BColor| = texelFetch(@|ColorTex|, ivec2(BCR, 0), 0);
}
