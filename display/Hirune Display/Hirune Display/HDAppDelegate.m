#import "HDAppDelegate.h"
#import "HDSplitView.h"
#import <Webkit/WebKit.h>

@implementation HDAppDelegate

NSMutableDictionary *views;

- (void)createView: (NSString*) label
{
    WebView* someView = [WebView new];
    [views setObject:someView forKey:label];
    return;
}

- (WebView*)getView: (NSString*) label
{
    return [views objectForKey:label];
}

- (void)changeView: (NSString*) label toURL: (NSString*) url
{
    [[[self getView: label] mainFrame]
     loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:url]]];
    return;
}

- (void)windowDidBecomeMain:(NSNotification *)notification
{
    static BOOL shouldGoFullScreen = YES;
    if (shouldGoFullScreen) {
        if (!([self.window styleMask] & NSFullScreenWindowMask)) {
            [self.window toggleFullScreen:nil];
        }
        shouldGoFullScreen = NO;
    }
}

- (void)windowDidEnterFullScreen:(NSNotification *)notification
{
    [self doResize: [[_window contentView] bounds].size];
    return;
}

- (NSSize)windowWillResize:(NSWindow *)sender toSize:(NSSize)frameSize
{
    [self doResize: frameSize];
    return frameSize;
}

CGFloat statusHeight;
HDSplitView *splitView;

- (void)doResize: (NSSize) size {
    CGFloat botPos = (size.height - 3*statusHeight);
    CGFloat topPos = statusHeight;
    
    [splitView setFrameSize: size];
    [splitView setNeedsDisplay: YES];
    [splitView adjustSubviews];
    [splitView setPosition:topPos ofDividerAtIndex:0];
    [splitView setPosition:botPos ofDividerAtIndex:1];
    return;
}

// http://stackoverflow.com/questions/7047250/building-a-server-client-application-in-cocoa
- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    NSMenu *mainMenu = [[NSApplication sharedApplication] mainMenu];
    statusHeight = [mainMenu menuBarHeight];

    views = [[NSMutableDictionary alloc] init];

    NSView* contentView = [_window contentView];
    NSRect initFrame = [contentView bounds];
    splitView = [[HDSplitView alloc] initWithFrame:initFrame];
    [contentView addSubview:splitView];
    
    [self createView: @"top"];
    [self createView: @"mid"];
    [self createView: @"bot"];
    
    [splitView addSubview:[self getView: @"top"]];
    [splitView addSubview:[self getView: @"mid"]];
    [splitView addSubview:[self getView: @"bot"]];
    [self doResize: initFrame.size];
    
    [self changeView: @"top" toURL: @"http://www.google.com/search?q=top"];
    [self changeView: @"mid" toURL: @"http://www.google.com/search?q=mid"];
    [self changeView: @"bot" toURL: @"http://www.google.com/search?q=bot"];
}

@end
