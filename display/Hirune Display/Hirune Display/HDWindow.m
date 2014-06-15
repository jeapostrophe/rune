#import "HDWindow.h"

@implementation HDWindow

- (BOOL)acceptsFirstMouse:(NSEvent *)e {
    return YES;
}
- (BOOL)acceptsFirstResponder
{
    return YES;
}
- (BOOL)becomeFirstResponder
{
    return YES;
}
- (BOOL)resignFirstResponder
{
    return YES;
}

-(void)mouseDown:(NSEvent *)e {
    NSLog(@"Mouse happen: %@", e);
}
-(void)keyDown:(NSEvent *)e {
    NSLog(@"Key happen: %@", e);
}

@end
