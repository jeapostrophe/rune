#version 130
uniform sampler2D @|BitmapTex|;
in vec2 @|TexCoord|;
out vec4 out_Color;

@|GLSL-Library|

void main ( void )
{
  out_Color = texture2DB(@|BitmapTex|, @|TexCoord|);
}
