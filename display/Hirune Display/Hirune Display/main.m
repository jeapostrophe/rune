#import <Cocoa/Cocoa.h>
#import "HDAppDelegate.h"

// xxx receive cmd-tab
int main(int argc, const char * argv[])
{
    NSApplication *application = [NSApplication sharedApplication];
    HDAppDelegate *appDelegate = [[HDAppDelegate alloc] init];
    
    [application setPresentationOptions: NSApplicationPresentationFullScreen];
    [application setDelegate:appDelegate];
    [application run];
    
    return EXIT_SUCCESS;
}
