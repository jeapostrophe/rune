#import "HDAppDelegate.h"
#import "HDWindow.h"

@implementation HDAppDelegate

- (id)init{
    self = [super init];
    if (!self) {
        return nil;
    }
    
    _window = [[HDWindow alloc] init];
    
    return self;
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    [_window getThisPartyStarted];
}

@end