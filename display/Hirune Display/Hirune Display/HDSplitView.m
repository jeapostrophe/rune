#import "HDSplitView.h"

@implementation HDSplitView

- (id)initWithFrame:(NSRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code here.
    }
    return self;
}

- (void)drawRect:(NSRect)dirtyRect
{
    [super drawRect:dirtyRect];
    
    // Drawing code here.
}

- (CGFloat)dividerThickness
{
    return 0.0;
}

@end
