//
//  main.m
//  Hirune Display
//
//  Created by Jay McCarthy on 6/15/14.
//  Copyright (c) 2014 Racket. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "HDAppDelegate.h"

int main(int argc, const char * argv[])
{
    NSApplication *application = [NSApplication sharedApplication];
    HDAppDelegate *appDelegate = [[HDAppDelegate alloc] init];
    
    [application setPresentationOptions: NSApplicationPresentationFullScreen];
    [application setDelegate:appDelegate];
    [application run];
    
    return EXIT_SUCCESS;
}
