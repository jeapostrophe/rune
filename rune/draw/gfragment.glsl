#version 130
uniform sampler2D @|FontTex|;

in vec4 @|FColor|;
in vec4 @|BColor|;
in vec2 @|TexCoord|;

out vec4 out_Color;

void main ( void )
{
  vec4 TexColor = texture2D(@|FontTex|, @|TexCoord|);

  // 40ms. I doubt you could do faster, because when i remove the if
  // and make it constant, it runs at the same speed.
  if (TexColor.r == 1.0) {
    out_Color = @|FColor|;
  } else {
    out_Color = @|BColor|;
  }
}
