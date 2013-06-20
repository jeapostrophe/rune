#version 130
uniform sampler2D @|BitmapTex|;
in vec2 @|TexCoord|;
out vec4 out_Color;

void main ( void )
{
  out_Color = texture2D(@|BitmapTex|, @|TexCoord|);
}
