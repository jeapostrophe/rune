#import "HDSplitView.h"

@implementation HDSplitView

- (CGFloat)dividerThickness
{
    return 0.0;
}

-(NSView *)hitTest:(NSPoint)aPoint
{
    return self;
}

@end
