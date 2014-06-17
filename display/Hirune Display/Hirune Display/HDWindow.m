#import <Webkit/WebKit.h>
#import "HDWindow.h"
#import "HDSplitView.h"
#import "GCDAsyncSocket.h"

@implementation HDWindow

// xxx change this
CGFloat statusHeight = 22.0;
// xxx make quad webviews
HDSplitView *splitView;

- (id)init{
    NSRect fullBounds = [[NSScreen mainScreen] frame];
    NSRect bounds = fullBounds;
    bounds.size.height *= 0.5;
    bounds.size.width *= 0.5;
    
    self = [super initWithContentRect:bounds
                           styleMask: (NSTitledWindowMask | NSClosableWindowMask | NSMiniaturizableWindowMask | NSResizableWindowMask)
                             backing:NSBackingStoreBuffered
                               defer:NO];
    if (!self) {
        return nil;
    }
    
    
    splitView = [[HDSplitView alloc] initWithFrame: bounds];
    
    [self setReleasedWhenClosed:YES];
    [self setAcceptsMouseMovedEvents:YES];
    [self setContentView: splitView];
    [self setCollectionBehavior:
     NSWindowCollectionBehaviorFullScreenPrimary];
    
    views = [[NSMutableDictionary alloc] init];
    
    return self;
}

- (void)getThisPartyStarted {
    [self makeKeyAndOrderFront:self];
    
    [self createView: @"top"];
    [self createView: @"mid"];
    [self createView: @"bot"];
    
    [splitView addSubview:[self getView: @"top"]];
    [splitView addSubview:[self getView: @"mid"]];
    [splitView addSubview:[self getView: @"bot"]];
    [self doResize];
    
    [self changeView: @"top" toURL: @"http://www.google.com/search?q=top"];
    [self changeView: @"mid" toURL: @"http://www.google.com/search?q=mid"];
    [self changeView: @"bot" toURL: @"http://www.google.com/search?q=bot"];
    
    [self initServer];

    [self toggleFullScreen:nil];
}

- (void)doResize {
    [self doResize: [[self contentView] bounds].size];
}

- (void)windowDidEnterFullScreen:(NSNotification *)notification {
    [self doResize];
}

- (NSSize)windowWillResize:(NSWindow *)sender toSize:(NSSize)frameSize
{
    [self doResize: frameSize];
    return frameSize;
}

- (void)doResize: (NSSize) size {
    CGFloat botPos = (size.height - 2*statusHeight);
    CGFloat topPos = statusHeight;
    
    [splitView setFrameSize: size];
    [splitView setNeedsDisplay: YES];
    [splitView adjustSubviews];
    [splitView setPosition:topPos ofDividerAtIndex:0];
    [splitView setPosition:botPos ofDividerAtIndex:1];
    return;
}

NSMutableDictionary *views;

- (void)createView: (NSString*) label
{
    WebView* someView = [WebView new];
    [views setObject:someView forKey:label];
}

- (WebView*)getView: (NSString*) label
{
    return [views objectForKey:label];
}

- (void)changeView: (NSString*) label toURL: (NSString*) url
{
    [[[self getView: label] mainFrame]
     loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:url]]];
}

- (BOOL)canBecomeKeyView {
    return  YES;
}
- (BOOL)acceptsFirstMouse:(NSEvent *)e {
    return YES;
}
- (BOOL)acceptsFirstResponder {
    return YES;
}
- (BOOL)becomeFirstResponder {
    return YES;
}
- (BOOL)resignFirstResponder {
    return YES;
}

-(void)mouseDown:(NSEvent *)e {
    // xxx send to manager
    NSLog(@"Mouse happen: %@", e);
}
-(void)keyDown:(NSEvent *)e {
    NSUInteger mods = [e modifierFlags];

    NSString *modss =
    [NSString stringWithFormat: @"%@%@%@%@",
     (mods & NSShiftKeyMask) ? @"S-" : @"",
     (mods & NSControlKeyMask) ? @"C-" : @"",
     (mods & NSAlternateKeyMask) ? @"M-" : @"",
     (mods & NSCommandKeyMask) ? @"Cmd-" : @""];

    NSString *rchars = [e charactersIgnoringModifiers];

    [self managerSend: [NSArray arrayWithObjects: @"key", modss, rchars, nil]];
}

// Manager API

- (void)managerRecv: (NSObject *)req {
    // xxx switch to array
    if(! [req isKindOfClass:[NSDictionary class]])
        return;
    
    NSDictionary *dict = (NSDictionary *)req;
    NSString *call = [dict objectForKey: @"call"];
    // xxx send js to view
    // xxx resize views
    if ( [call isEqualToString: @"url"] ) {
        [self changeView: [dict objectForKey: @"view"] toURL: [dict objectForKey: @"url"] ];
    }
}

- (void)managerSend: (NSObject *)req {
    if ( ! mainSock )
        return;
    
    NSError *error = nil;
    NSData *data = [NSJSONSerialization dataWithJSONObject:req options:0 error:&error];
    if ( data ) {
        NSLog(@"Send: %@", req);
        [mainSock writeData:data withTimeout:-1 tag:0];
    } else {
        NSLog(@"Send Error: %@", error);
    }
}

// Server

dispatch_queue_t socketQueue;
GCDAsyncSocket *listenSocket;
GCDAsyncSocket *mainSock;

- (void)initServer {
    socketQueue = dispatch_queue_create("socketQueue", NULL);
    
    listenSocket = [[GCDAsyncSocket alloc] initWithDelegate:self delegateQueue:socketQueue];
    NSError *error = nil;
    int port = 7331;
    if(![listenSocket acceptOnPort:port error:&error])
    {
        NSLog(@"Error starting server: %@", error);
        [NSApp terminate:self];
    }
    
    NSLog(@"Server started on port %hu", [listenSocket localPort]);
}

- (void)socket:(GCDAsyncSocket *)sock didAcceptNewSocket:(GCDAsyncSocket *)newSocket
{
    if ( mainSock ) {
        [newSocket disconnect];
        return;
    }
    
    dispatch_async(dispatch_get_main_queue(), ^{
		@autoreleasepool {
            NSLog(@"Accept");
        }
    });
    
    mainSock = newSocket;
    [mainSock readDataToData:[GCDAsyncSocket CRLFData] withTimeout:-1 tag:0];
}

- (void)socket:(GCDAsyncSocket *)sock didWriteDataWithTag:(long)tag
{
    dispatch_async(dispatch_get_main_queue(), ^{
		@autoreleasepool {
            NSLog(@"Write");
        }
    });
}

- (void)socket:(GCDAsyncSocket *)sock didReadData:(NSData *)data withTag:(long)tag
{
	dispatch_async(dispatch_get_main_queue(), ^{
		@autoreleasepool {
            NSLog(@"Read");
            NSError *error = nil;
            NSObject *req =
            [NSJSONSerialization
             JSONObjectWithData: [data subdataWithRange:NSMakeRange(0, [data length] - 2)]
             options: 0
             error: &error];
            if ( req ) {
                NSLog(@"Recv: %@", req);
                [self managerRecv: req];
            } else {
                NSLog(@"Recv Error: %@", error);
            }
		}
	});
    [sock readDataToData:[GCDAsyncSocket CRLFData] withTimeout:-1 tag:0];
}

- (void)socketDidDisconnect:(GCDAsyncSocket *)sock withError:(NSError *)err
{
    dispatch_async(dispatch_get_main_queue(), ^{
		@autoreleasepool {
            NSLog(@"Disconnect");
        }
    });
	if (sock != listenSocket && sock == mainSock) {
        mainSock = nil;
    }
}

@end
