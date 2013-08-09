#version 130
uniform sampler2D @|FontTex|;

in vec4 @|FColor|;
in vec4 @|BColor|;
in vec2 @|TexCoord|;

out vec4 out_Color;

@|GLSL-Library|

void main ( void )
{
  vec4 TexColor = texture2DB(@|FontTex|, @|TexCoord|);

  // using r just because the texture is just monochrome.
  // Optimization Idea: maybe I should use GL_RED on the texture?
  float x = TexColor.r;
  // Lagrange polynomial of: (0.0, @|BColor|) (1.0, @|FColor|);
  float x0 = 0.0;
  float x1 = 1.0;
  float ell0 = (x - x1) / (x0 - x1);
  float ell1 = (x - x0) / (x1 - x0);
  out_Color = (@|BColor| * ell0) + (@|FColor| * ell1);
}
