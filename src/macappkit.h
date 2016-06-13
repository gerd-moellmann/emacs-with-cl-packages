/* Definitions and headers for AppKit framework on the Mac OS.
   Copyright (C) 2008-2016  YAMAMOTO Mitsuharu

This file is part of GNU Emacs Mac port.

GNU Emacs Mac port is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs Mac port is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs Mac port.  If not, see <http://www.gnu.org/licenses/>.  */

#undef Z
#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>
#import <Quartz/Quartz.h>
#import <QuartzCore/QuartzCore.h>
#import <IOKit/graphics/IOGraphicsLib.h>
#import <OSAKit/OSAKit.h>
#define Z (current_buffer->text->z)

#ifndef NSFoundationVersionNumber10_8_3
#define NSFoundationVersionNumber10_8_3 945.16
#endif

#ifndef NSAppKitVersionNumber10_6
#define NSAppKitVersionNumber10_6 1038
#endif
#ifndef NSAppKitVersionNumber10_7
#define NSAppKitVersionNumber10_7 1138
#endif
#ifndef NSAppKitVersionNumber10_8
#define NSAppKitVersionNumber10_8 1187
#endif
#ifndef NSAppKitVersionNumber10_9
#define NSAppKitVersionNumber10_9 1265
#endif
#ifndef NSAppKitVersionNumber10_10_Max
#define NSAppKitVersionNumber10_10_Max 1349
#endif

#ifndef USE_ARC
#if defined (__clang__) && __has_feature (objc_arc)
#define USE_ARC 1
#endif
#endif

#if !USE_ARC
#ifndef __unsafe_unretained
#define __unsafe_unretained
#endif
#ifndef __autoreleasing
#define __autoreleasing
#endif
#endif

#if !__has_feature (objc_instancetype)
typedef id instancetype;
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 101100 && __has_feature (objc_generics)
#define NSArrayOf(ObjectType)		NSArray <ObjectType>
#define NSMutableArrayOf(ObjectType)	NSMutableArray <ObjectType>
#define NSSetOf(ObjectType)		NSSet <ObjectType>
#define NSMutableSetOf(ObjectType)	NSMutableSet <ObjectType>
#define NSDictionaryOf(KeyT, ObjectT)	NSDictionary <KeyT, ObjectT>
#define NSMutableDictionaryOf(KeyT, ObjectT) NSMutableDictionary <KeyT, ObjectT>
#else
#define NSArrayOf(ObjectType)		NSArray
#define NSMutableArrayOf(ObjectType)	NSMutableArray
#define NSSetOf(ObjectType)		NSSet
#define NSMutableSetOf(ObjectType)	NSMutableSet
#define NSDictionaryOf(KeyT, ObjectT)	NSDictionary
#define NSMutableDictionaryOf(KeyT, ObjectT) NSMutableDictionary
#endif

@interface NSData (Emacs)
- (Lisp_Object)lispString;
@end

@interface NSString (Emacs)
+ (instancetype)stringWithLispString:(Lisp_Object)lispString;
+ (instancetype)stringWithUTF8LispString:(Lisp_Object)lispString;
+ (instancetype)stringWithUTF8String:(const char *)bytes fallback:(BOOL)flag;
- (Lisp_Object)lispString;
- (Lisp_Object)UTF8LispString;
- (Lisp_Object)UTF16LispString;
- (NSArrayOf (NSString *) *)componentsSeparatedByCamelCasingWithCharactersInSet:(NSCharacterSet *)separator;
@end

@interface NSFont (Emacs)
+ (NSFont *)fontWithFace:(struct face *)face;
@end

@interface NSEvent (Emacs)
- (NSEvent *)mouseEventByChangingType:(NSEventType)type
		          andLocation:(NSPoint)location;
- (CGEventRef)coreGraphicsEvent;
@end

@interface NSAttributedString (Emacs)
- (Lisp_Object)UTF16LispString;
@end

@interface NSColor (Emacs)
+ (NSColor *)colorWithXColorPixel:(unsigned long)pixel;
- (CGColorRef)copyCGColor;
@end

@interface NSImage (Emacs)
+ (NSImage *)imageWithCGImage:(CGImageRef)cgImage exclusive:(BOOL)flag;
@end

@interface NSApplication (Emacs)
- (void)postDummyEvent;
- (void)runTemporarilyWithBlock:(void (^)(void))block;
@end

@interface NSScreen (Emacs)
+ (NSScreen *)screenContainingPoint:(NSPoint)aPoint;
+ (NSScreen *)closestScreenForRect:(NSRect)aRect;
- (BOOL)containsDock;
- (BOOL)canShowMenuBar;
@end

@interface NSCursor (Emacs)
+ (NSCursor *)cursorWithThemeCursor:(ThemeCursor)shape;
@end

#if MAC_OS_X_VERSION_MIN_REQUIRED < 101000
/* Workarounds for memory leaks on OS X 10.9.  */
@interface NSApplication (Undocumented)
- (void)_installMemoryPressureDispatchSources;
- (void)_installMemoryStatusDispatchSources;
@end
#endif

@interface EmacsApplication : NSApplication
@end

@interface EmacsPosingWindow : NSWindow
+ (void)setup;
@end

/* Class for delegate for NSApplication.  It also becomes the target
   of several actions such as those from EmacsMainView, menus,
   dialogs, and actions/services bound in the mac-apple-event
   keymap.  */

@interface EmacsController : NSObject <NSApplicationDelegate>
{
  /* Points to HOLD_QUIT arg passed to read_socket_hook.  */
  struct input_event *hold_quit;

  /* Number of events stored during a
     handleQueuedEventsWithHoldingQuitIn: call.  */
  int count;

  /* Whether to generate a HELP_EVENT at the end of handleOneNSEvent:
     call.  */
  int do_help;

  /* Non-zero means that a HELP_EVENT has been generated since Emacs
   start.  */
  bool any_help_event_p;

  /* The frame on which a HELP_EVENT occurs.  */
  struct frame *emacsHelpFrame;

  /* The item selected in the popup menu.  */
  int menuItemSelection;

  /* Non-nil means left mouse tracking has been suspended and will be
     resumed when this block is called.  */
  void (^trackingResumeBlock)(void);

  /* Whether a service provider for Emacs is registered as of
     applicationWillFinishLaunching: or not.  */
  BOOL serviceProviderRegistered;

  /* Whether the application should update its presentation options
     when it becomes active next time.  */
  BOOL needsUpdatePresentationOptionsOnBecomingActive;

  /* Whether conflicting Cocoa's text system key bindings (e.g., C-q)
     are disabled or not.  */
  BOOL conflictingKeyBindingsDisabled;

  /* Saved key bindings with or without conflicts (currently, those
     for writing direction commands on Mac OS X 10.6).  */
  NSDictionaryOf (NSString *, NSString *)
    *keyBindingsWithConflicts, *keyBindingsWithoutConflicts;

  /* Help topic that the user selected using Help menu search.  */
  id selectedHelpTopic;

  /* Search string for which the user selected "Show All Help
     Topics".  */
  NSString *searchStringForAllHelpTopics;

  /* Date of last flushWindow call.  */
  NSDate *lastFlushDate;

  /* Timer for deferring flushWindow call.  */
  NSTimer *flushTimer;

  /* Set of windows whose flush is deferred.  */
  NSMutableSetOf (NSWindow *) *deferredFlushWindows;
}
- (int)getAndClearMenuItemSelection;
- (void)storeInputEvent:(id)sender;
- (void)setMenuItemSelectionToTag:(id)sender;
- (void)storeEvent:(struct input_event *)bufp;
- (void)setTrackingResumeBlock:(void (^)(void))block;
- (NSTimeInterval)minimumIntervalForReadSocket;
- (int)handleQueuedNSEventsWithHoldingQuitIn:(struct input_event *)bufp;
- (void)cancelHelpEchoForEmacsFrame:(struct frame *)f;
- (BOOL)conflictingKeyBindingsDisabled;
- (void)setConflictingKeyBindingsDisabled:(BOOL)flag;
- (void)flushWindow:(NSWindow *)window force:(BOOL)flag;
- (void)updatePresentationOptions;
- (void)showMenuBar;
@end

/* Like NSWindow, but allows suspend/resume resize control tracking.  */

@interface EmacsWindow : NSWindow
{
  /* Left mouse up event used for suspending resize control
     tracking.  */
  NSEvent *mouseUpEvent;

  /* Pointer location of the left mouse down event that initiated the
     current resize control tracking session.  The value is in the
     base coordinate system of the window.  */
  NSPoint resizeTrackingStartLocation;

  /* Window size when the current resize control tracking session was
     started.  */
  NSSize resizeTrackingStartWindowSize;

  /* Event number of the current resize control tracking session.
     Don't compare this with the value of a drag event: the latter is
     is always 0 if the event comes via Screen Sharing.  */
  NSInteger resizeTrackingEventNumber;

  /* Whether the window should be made visible when the application
     gets unhidden next time.  */
  BOOL needsOrderFrontOnUnhide;

  /* Whether to suppress the usual -constrainFrameRect:toScreen:
     behavior.  */
  BOOL constrainingToScreenSuspended;
}
- (void)suspendResizeTracking:(NSEvent *)event
	   positionAdjustment:(NSPoint)adjustment;
- (void)resumeResizeTracking;
- (BOOL)needsOrderFrontOnUnhide;
- (void)setNeedsOrderFrontOnUnhide:(BOOL)flag;
- (void)setConstrainingToScreenSuspended:(BOOL)flag;
@end

@interface EmacsFullscreenWindow : EmacsWindow
@end

@interface NSObject (EmacsWindowDelegate)
- (BOOL)window:(NSWindow *)sender shouldForwardAction:(SEL)action to:(id)target;
- (NSRect)window:(NSWindow *)sender willConstrainFrame:(NSRect)frameRect
	toScreen:(NSScreen *)screen;
@end

@class EmacsView;
@class EmacsOverlayView;

/* Class for delegate of NSWindow and NSToolbar (see its Toolbar
   category declared later).  It also becomes that target of
   frame-dependent actions such as those from font panels.  */

@interface EmacsFrameController : NSObject <NSWindowDelegate>
{
  /* The Emacs frame corresponding to the NSWindow that
     EmacsFrameController object is associated with as delegate.  */
  struct frame *emacsFrame;

  /* Window and view for the Emacs frame.  */
  EmacsWindow *emacsWindow;
  EmacsView *emacsView;

  /* Window and view overlaid on the Emacs frame window.  */
  NSWindow *overlayWindow;
  EmacsOverlayView *overlayView;

  /* The spinning progress indicator (corresponding to hourglass)
     shown at the upper-right corner of the window.  */
  NSProgressIndicator *hourglass;

  /* The current window manager state.  */
  WMState windowManagerState;

  /* The last window frame before maximize/fullscreen.  The position
     is relative to the top left corner of the screen.  */
  NSRect savedFrame;

  /* The view hosting Core Animation layers in the overlay window.  */
  NSView *layerHostingView;

  /* The block called when the window ends live resize.  */
  void (^liveResizeCompletionHandler) (void);

  /* Whether transition effect should be set up when the window will
     start live resize.  */
  BOOL shouldLiveResizeTriggerTransition;

  /* Window manager state after the full screen transition.  */
  WMState fullScreenTargetState;

  /* Enum value determining the symbol that is set as `fullscreen'
     frame parameter after the full screen transition.  */
  enum {
    FULLSCREEN_PARAM_NONE, FULLSCREEN_PARAM_NIL,
    FULLSCREEN_PARAM_FULLBOTH, FULLSCREEN_PARAM_FULLSCREEN
  } fullscreenFrameParameterAfterTransition;

  /* Array of blocks called when the window completes full screen
     transition.  Each block is called with the window object and a
     boolean value meaning whether the transition has succeeded.  */
  NSMutableArrayOf (void (^)(EmacsWindow *, BOOL))
    *fullScreenTransitionCompletionHandlers;
}
- (instancetype)initWithEmacsFrame:(struct frame *)emacsFrame;
- (void)setupEmacsView;
- (void)setupWindow;
- (struct frame *)emacsFrame;
- (EmacsWindow *)emacsWindow;
- (WMState)windowManagerState;
- (void)setWindowManagerState:(WMState)newState;
- (void)updateBackingScaleFactor;
- (BOOL)emacsViewCanDraw;
- (void)lockFocusOnEmacsView;
- (void)unlockFocusOnEmacsView;
- (void)scrollEmacsViewRect:(NSRect)aRect by:(NSSize)offset;
- (NSPoint)convertEmacsViewPointToScreen:(NSPoint)point;
- (NSPoint)convertEmacsViewPointFromScreen:(NSPoint)point;
- (NSRect)convertEmacsViewRectToScreen:(NSRect)rect;
- (NSRect)centerScanEmacsViewRect:(NSRect)rect;
- (void)invalidateCursorRectsForEmacsView;
#if MAC_OS_X_VERSION_MIN_REQUIRED < 101000
- (void)maskRoundedBottomCorners:(NSRect)clipRect directly:(BOOL)flag;
#endif
- (NSBitmapImageRep *)bitmapImageRepInContentViewRect:(NSRect)rect;
- (void)storeModifyFrameParametersEvent:(Lisp_Object)alist;
- (BOOL)isWindowFrontmost;
- (void)setupLiveResizeTransition;
- (void)setShouldLiveResizeTriggerTransition:(BOOL)flag;
- (void)setLiveResizeCompletionHandler:(void (^)(void))block;
@end

/* Class for Emacs view that handles drawing events only.  It is used
   directly by tooltip frames, and indirectly by ordinary frames via
   inheritance.  */

@interface EmacsView : NSView
@end

/* Class for Emacs view that also handles input events.  Used by
   ordinary frames.  */

@interface EmacsMainView : EmacsView <NSTextInputClient>
{
  /* Target object to which the EmacsMainView object sends
     actions.  */
  id __unsafe_unretained target;

  /* Message selector of the action the EmacsMainView object
     sends.  */
  SEL action;

  /* Stores the Emacs input event that the action method is expected
     to process.  */
  struct input_event inputEvent;

  /* Whether key events were interpreted by intepretKeyEvents:.  */
  BOOL keyEventsInterpreted;

#if MAC_OS_X_VERSION_MIN_REQUIRED < 101000
  /* Whether scrollRect:by: has copied rounded bottom corner area.  */
  BOOL roundedBottomCornersCopied;
#endif

  /* Whether the raw key event below has mapped any of CGEvent flags.
     It is precalculated in keyDown: so as to avoid regeneration of a
     CGEvent object.  */
  BOOL rawKeyEventHasMappedFlags;

  /* Raw key event that is interpreted by intepretKeyEvents:.  */
  NSEvent *rawKeyEvent;

  /* Saved marked text passed by setMarkedText:selectedRange:.  */
  id markedText;

  /* Position in the last normal (non-momentum) wheel event.  */
  NSPoint savedWheelPoint;

  /* Modifiers in the last normal (non-momentum) wheel event.  */
  int savedWheelModifiers;

  /* Stage of the last gesture event of type NSEventTypePressure.  */
  NSInteger pressureEventStage;
}
- (struct frame *)emacsFrame;
- (id)target;
- (SEL)action;
- (void)setTarget:(id)anObject;
- (void)setAction:(SEL)aSelector;
- (BOOL)sendAction:(SEL)theAction to:(id)theTarget;
- (struct input_event *)inputEvent;
- (NSString *)string;
- (NSRect)firstRectForCharacterRange:(NSRange)aRange
			 actualRange:(NSRangePointer)actualRange;
@end

/* Class for view in the overlay window of an Emacs frame window.  */

@interface EmacsOverlayView : NSView
{
  /* Whether to highlight the area corresponding to the content of the
     Emacs frame window.  */
  BOOL highlighted;

  /* Whether to show the resize indicator.  */
  BOOL showsResizeIndicator;
}
- (void)setHighlighted:(BOOL)flag;
- (void)setShowsResizeIndicator:(BOOL)flag;
- (void)adjustWindowFrame;
@end

/* Class for view used in live resize transition animations.  */

@interface EmacsLiveResizeTransitionView : NSView
@end

/* Class for scroller that doesn't do modal mouse tracking.  */

@interface NonmodalScroller : NSScroller
{
  /* Timer used for posting events periodically during mouse
     tracking.  */
  NSTimer *timer;

  /* Code for the scroller part that the user hit.  */
  NSScrollerPart hitPart;

  /* Whether the hitPart area should be highlighted.  */
  BOOL hilightsHitPart;

  /* If the scroller knob is currently being dragged by the user, this
     is the number of pixels from the top of the knob to the place
     where the user grabbed it.  If the knob is pressed but not
     dragged yet, this is a negative number whose absolute value is
     the number of pixels plus 1.  */
  CGFloat knobGrabOffset;

  /* The position of the top (for vertical scroller) or left (for
     horizontal, respectively) of the scroller knob in pixels,
     relative to the knob slot.  */
  CGFloat knobMinEdgeInSlot;
}
+ (void)updateBehavioralParameters;
- (BOOL)dragUpdatesFloatValue;
- (NSTimeInterval)buttonDelay;
- (NSTimeInterval)buttonPeriod;
- (BOOL)pagingBehavior;
@end

/* Just for avoiding warnings about undocumented methods in NSScroller.  */

@interface NSScroller (Undocumented)
- (void)drawArrow:(NSUInteger)position highlightPart:(NSInteger)part;
@end

/* Class for Scroller used for an Emacs window.  */

@interface EmacsScroller : NonmodalScroller
{
  /* Emacs scroll bar for the scroller.  */
  struct scroll_bar *emacsScrollBar;

  /* The size of the scroller knob track area in pixels.  */
  CGFloat knobSlotSpan;

  /* Minimum size of the scroller knob, in pixels.  */
  CGFloat minKnobSpan;

  /* The size of the whole scroller area in pixels.  */
  CGFloat frameSpan;

  /* The position the user clicked in pixels, relative to the whole
     scroller area.  */
  CGFloat clickPositionInFrame;

  /* This is used for saving the `code' and `modifiers' members of an
     input event for a scroller click with the control modifier.  */
  struct input_event inputEvent;

  /* Values in the last mac_set_scroll_bar_thumb call.  */
  int whole, portion;
}
- (void)setEmacsScrollBar:(struct scroll_bar *)bar;
- (struct scroll_bar *)emacsScrollBar;
- (CGFloat)knobSlotSpan;
- (CGFloat)minKnobSpan;
- (CGFloat)knobMinEdgeInSlot;
- (CGFloat)frameSpan;
- (CGFloat)clickPositionInFrame;
- (ptrdiff_t)inputEventCode;
- (int)inputEventModifiers;
- (int)whole;
- (void)setWhole:(int)theWhole;
- (int)portion;
- (void)setPortion:(int)thePortion;
@end

@interface EmacsFrameController (ScrollBar)
- (void)addScrollerWithScrollBar:(struct scroll_bar *)bar;
@end

@interface EmacsToolbarItem : NSToolbarItem
{
  /* Array of CoreGraphics images of the item.  */
  NSArrayOf (id) *coreGraphicsImages;
}
- (void)setCoreGraphicsImage:(CGImageRef)cgImage;
- (void)setCoreGraphicsImages:(NSArrayOf (id) *)cgImages;
@end

/* Dummy protocol for specifying the return type of the selector
   `item' that has multiple possibilities if unspecified.  */
@protocol EmacsToolbarItemViewer
- (NSToolbarItem *)item;
@end

@interface EmacsFrameController (Toolbar) <NSToolbarDelegate>
- (void)setupToolBarWithVisibility:(BOOL)visible;
- (void)updateToolbarDisplayMode;
- (void)storeToolBarEvent:(id)sender;
- (void)noteToolBarMouseMovement:(NSEvent *)event;
@end

/* Like NSFontPanel, but allows suspend/resume slider tracking.  */

@interface EmacsFontPanel : NSFontPanel
{
  /* Left mouse up event used for suspending slider tracking.  */
  NSEvent *mouseUpEvent;

  /* Slider being tracked.  */
  NSSlider * __unsafe_unretained trackedSlider;
}
- (void)suspendSliderTracking:(NSEvent *)event;
- (void)resumeSliderTracking;
@end

@interface EmacsController (FontPanel)
- (void)fontPanelWillClose:(NSNotification *)notification;
@end

@interface EmacsFrameController (FontPanel)
- (NSFont *)fontForFace:(int)faceId character:(int)c
	       position:(int)pos object:(Lisp_Object)object;
- (void)changeFont:(id)sender;
@end

@interface EmacsFrameController (EventHandling)
- (void)noteEnterEmacsView;
- (void)noteLeaveEmacsView;
- (BOOL)noteMouseMovement:(NSPoint)point;
@end

@interface EmacsFrameController (Hourglass)
- (void)showHourglass:(id)sender;
- (void)hideHourglass:(id)sender;
@end

@interface EmacsSavePanel : NSSavePanel
@end

@interface EmacsOpenPanel : NSOpenPanel
@end

@interface EmacsFontDialogController : NSObject <NSWindowDelegate>
@end

@interface NSFontPanel (Emacs)
- (NSInteger)runModal;
@end

@interface NSMenu (Emacs)
- (NSMenuItem *)addItemWithWidgetValue:(widget_value *)wv;
- (void)fillWithWidgetValue:(widget_value *)first_wv;
@end

@interface EmacsMenu : NSMenu
@end

@interface NSEvent (Undocumented)
- (BOOL)_continuousScroll;
- (NSInteger)_scrollPhase;
- (CGFloat)deviceDeltaX;
- (CGFloat)deviceDeltaY;
- (CGFloat)deviceDeltaZ;
@end

@interface EmacsController (Menu) <NSMenuDelegate, NSUserInterfaceItemSearching>
- (void)trackMenuBar;
@end

@interface EmacsFrameController (Menu)
- (void)popUpMenu:(NSMenu *)menu atLocationInEmacsView:(NSPoint)location;
@end

@interface EmacsDialogView : NSView
- (instancetype)initWithWidgetValue:(widget_value *)wv;
@end

@interface EmacsPrintProxyView : NSView
{
  NSArrayOf (NSView *) *views;
}
- (instancetype)initWithViews:(NSArrayOf (NSView *) *)theViews;
@end

@interface NSPasteboard (Emacs)
- (BOOL)setLispObject:(Lisp_Object)lispObject forType:(NSString *)dataType;
- (Lisp_Object)lispObjectForType:(NSString *)dataType;
@end

@interface NSAppleEventDescriptor (Emacs)
- (OSErr)copyDescTo:(AEDesc *)desc;
@end

@interface EmacsFrameController (DragAndDrop)
- (void)registerEmacsViewForDraggedTypes:(NSArrayOf (NSString *) *)pboardTypes;
- (void)setOverlayViewHighlighted:(BOOL)flag;
@end

@interface EmacsOSAScript : OSAScript
@end

@interface DOMSVGRect : DOMObject
- (float)x;
- (float)y;
- (float)width;
- (float)height;
@end

@interface NSView (Emacs)
- (XImagePtr)createXImageFromRect:(NSRect)rect backgroundColor:(NSColor *)color
		      scaleFactor:(CGFloat)scaleFactor;
@end

/* Class for SVG frame load delegate.  */
@interface EmacsSVGLoader : NSObject
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 101100
			    <WebFrameLoadDelegate>
#endif
{
  /* Frame and image data structures to which the SVG image is
     loaded.  */
  struct frame *emacsFrame;
  struct image *emacsImage;

  /* Function called when checking image size.  */
  bool (*checkImageSizeFunc) (struct frame *, int, int);

  /* Function called when reporting image load errors.  */
  void (*imageErrorFunc) (const char *, ...);

  /* Whether a page load has completed.  */
  BOOL isLoaded;
}
- (instancetype)initWithEmacsFrame:(struct frame *)f emacsImage:(struct image *)img
		checkImageSizeFunc:(bool (*)(struct frame *, int, int))checkImageSize
		    imageErrorFunc:(void (*)(const char *, ...))imageError;
- (bool)loadData:(NSData *)data backgroundColor:(NSColor *)backgroundColor;
@end

/* Protocol for document rasterization.  */

@protocol EmacsDocumentRasterizer <NSObject>
- (instancetype)initWithURL:(NSURL *)url
		    options:(NSDictionaryOf (NSString *, id) *)options;
- (instancetype)initWithData:(NSData *)data
		     options:(NSDictionaryOf (NSString *, id) *)options;
+ (NSArrayOf (NSString *) *)supportedTypes;
- (NSUInteger)pageCount;
- (NSSize)integralSizeOfPageAtIndex:(NSUInteger)index;
- (CGColorRef)copyBackgroundCGColorOfPageAtIndex:(NSUInteger)index;
- (NSDictionaryOf (NSString *, id) *)documentAttributesOfPageAtIndex:(NSUInteger)index;
- (void)drawPageAtIndex:(NSUInteger)index inRect:(NSRect)rect
	      inContext:(CGContextRef)ctx;
@end

/* Class for PDF rasterization.  */

@interface EmacsPDFDocument : PDFDocument <EmacsDocumentRasterizer>
@end

/* Class for document rasterization other than PDF.  It also works as
   the layout manager delegate when rasterizing a multi-page
   document.  */

@interface EmacsDocumentRasterizer : NSObject <EmacsDocumentRasterizer, NSLayoutManagerDelegate>
{
  /* The text storage and document attributes for the document to be
     rasterized.  */
  NSTextStorage *textStorage;
  NSDictionaryOf (NSString *, id) *documentAttributes;
}
- (instancetype)initWithAttributedString:(NSAttributedString *)anAttributedString
		      documentAttributes:(NSDictionaryOf (NSString *, id) *)docAttributes;
@end

@interface EmacsFrameController (Accessibility)
- (void)postAccessibilityNotificationsToEmacsView;
@end

@interface EmacsFrameController (Animation)
- (void)setupLayerHostingView;
- (CALayer *)layerForRect:(NSRect)rect;
- (void)addLayer:(CALayer *)layer;
- (CIFilter *)transitionFilterFromProperties:(Lisp_Object)properties;
- (void)adjustTransitionFilter:(CIFilter *)filter forLayer:(CALayer *)layer;
@end

@interface NSLayoutManager (Emacs)
- (NSRect)enclosingRectForGlyphRange:(NSRange)glyphRange
		     inTextContainer:(NSTextContainer *)textContainer;
@end

@interface EmacsController (Sound) <NSSoundDelegate>
@end

/* Some methods that are not declared in older versions.  Should be
   used with some runtime check such as `respondsToSelector:'. */

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1070
@interface NSColor (AvailableOn1070AndLater)
+ (NSColor *)colorWithSRGBRed:(CGFloat)red green:(CGFloat)green
			 blue:(CGFloat)blue alpha:(CGFloat)alpha;
@end
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1080
@interface NSColor (AvailableOn1080AndLater)
- (CGColorRef)CGColor;
@end
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1080
@interface NSFileManager (AvailableOn1080AndLater)
- (BOOL)trashItemAtURL:(NSURL *)url resultingItemURL:(NSURL **)outResultingURL
		 error:(NSError **)error;
@end
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1070
enum {
  NSApplicationPresentationFullScreen			= 1 << 10,
  NSApplicationPresentationAutoHideToolbar		= 1 << 11
};
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1090
enum {
  NSModalResponseAbort		= NSRunAbortedResponse,
  NSModalResponseContinue	= NSRunContinuesResponse
};

enum {
  NSModalResponseOK	= NSOKButton
};
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1070
enum {
  NSWindowCollectionBehaviorFullScreenPrimary	= 1 << 7,
  NSWindowCollectionBehaviorFullScreenAuxiliary	= 1 << 8
};
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1070
enum {
  NSWindowAnimationBehaviorDefault		= 0,
  NSWindowAnimationBehaviorNone			= 2,
  NSWindowAnimationBehaviorDocumentWindow	= 3,
  NSWindowAnimationBehaviorUtilityWindow	= 4,
  NSWindowAnimationBehaviorAlertPanel		= 5
};
typedef NSInteger NSWindowAnimationBehavior;

enum {
  NSFullScreenWindowMask = 1 << 14
};

@interface NSWindow (AvailableOn1070AndLater)
- (NSWindowAnimationBehavior)animationBehavior;
- (void)setAnimationBehavior:(NSWindowAnimationBehavior)newAnimationBehavior;
- (void)toggleFullScreen:(id)sender;
- (CGFloat)backingScaleFactor;
@end
#endif

#if MAC_OS_X_VERSION_MIN_REQUIRED < 101000
@interface NSWindow (Undocumented)
- (NSRect)_intersectBottomCornersWithRect:(NSRect)viewRect;
- (void)_maskRoundedBottomCorners:(NSRect)clipRect;
@end
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 101200
enum {
  NSWindowStyleMaskBorderless		= NSBorderlessWindowMask,
  NSWindowStyleMaskTitled		= NSTitledWindowMask,
  NSWindowStyleMaskClosable		= NSClosableWindowMask,
  NSWindowStyleMaskMiniaturizable	= NSMiniaturizableWindowMask,
  NSWindowStyleMaskResizable		= NSResizableWindowMask,
  NSWindowStyleMaskFullScreen		= NSFullScreenWindowMask
};
typedef NSUInteger NSWindowStyleMask;
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1070
@interface NSScreen (AvailableOn1070AndLater)
- (CGFloat)backingScaleFactor;
@end
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1090
@interface NSScreen (AvailableOn1090AndLater)
+ (BOOL)screensHaveSeparateSpaces;
@end
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1090
@interface NSSavePanel (AvailableOn1090AndLater)
- (void)setShowsTagField:(BOOL)flag;
@end
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1070
enum {
    NSEventPhaseNone        = 0,
    NSEventPhaseBegan       = 0x1 << 0,
    NSEventPhaseStationary  = 0x1 << 1,
    NSEventPhaseChanged     = 0x1 << 2,
    NSEventPhaseEnded       = 0x1 << 3,
    NSEventPhaseCancelled   = 0x1 << 4,
};
typedef NSUInteger NSEventPhase;

@interface NSEvent (AvailableOn1070AndLater)
- (BOOL)hasPreciseScrollingDeltas;
- (CGFloat)scrollingDeltaX;
- (CGFloat)scrollingDeltaY;
- (NSEventPhase)momentumPhase;
- (BOOL)isDirectionInvertedFromDevice;
- (NSEventPhase)phase;
+ (BOOL)isSwipeTrackingFromScrollEventsEnabled;
@end
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1080
enum {
    NSEventTypeSmartMagnify = 32
};

enum {
    NSEventPhaseMayBegin    = 0x1 << 5
};
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 101100
@interface NSEvent (AvailableOn101003AndLater)
- (NSInteger)stage;
@end
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 101200
enum {
  NSEventTypeLeftMouseDown	= NSLeftMouseDown,
  NSEventTypeLeftMouseUp	= NSLeftMouseUp,
  NSEventTypeMouseMoved		= NSMouseMoved,
  NSEventTypeLeftMouseDragged	= NSLeftMouseDragged,
  NSEventTypeKeyDown		= NSKeyDown,
  NSEventTypeKeyUp		= NSKeyUp,
  NSEventTypeApplicationDefined	= NSApplicationDefined,
  NSEventTypeScrollWheel	= NSScrollWheel
};

enum {
  NSEventMaskLeftMouseDown	= NSLeftMouseDownMask,
  NSEventMaskLeftMouseUp	= NSLeftMouseUpMask,
  NSEventMaskRightMouseDown	= NSRightMouseDownMask,
  NSEventMaskRightMouseUp	= NSRightMouseUpMask,
  NSEventMaskMouseMoved		= NSMouseMovedMask,
  NSEventMaskLeftMouseDragged	= NSLeftMouseDraggedMask,
  NSEventMaskRightMouseDragged	= NSRightMouseDraggedMask,
  NSEventMaskMouseEntered	= NSMouseEnteredMask,
  NSEventMaskMouseExited	= NSMouseExitedMask,
  NSEventMaskKeyDown		= NSKeyDownMask,
  NSEventMaskKeyUp		= NSKeyUpMask,
  NSEventMaskScrollWheel	= NSScrollWheelMask,
  NSEventMaskOtherMouseDown	= NSOtherMouseDownMask,
  NSEventMaskOtherMouseUp	= NSOtherMouseUpMask,
  NSEventMaskOtherMouseDragged	= NSOtherMouseDraggedMask,
  NSEventMaskAny		= NSAnyEventMask
};

enum {
  NSEventModifierFlagCapsLock	= NSAlphaShiftKeyMask,
  NSEventModifierFlagShift	= NSShiftKeyMask,
  NSEventModifierFlagControl	= NSControlKeyMask,
  NSEventModifierFlagOption	= NSAlternateKeyMask,
  NSEventModifierFlagCommand	= NSCommandKeyMask,
  NSEventModifierFlagNumericPad	= NSNumericPadKeyMask,
  NSEventModifierFlagHelp	= NSHelpKeyMask,
  NSEventModifierFlagFunction	= NSFunctionKeyMask
};
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1070
@interface NSAnimationContext (AvailableOn1070AndLater)
+ (void)runAnimationGroup:(void (^)(NSAnimationContext *context))changes
        completionHandler:(void (^)(void))completionHandler;
- (void)setTimingFunction:(CAMediaTimingFunction *)newTimingFunction;
@end

@interface CALayer (AvailableOn1070AndLater)
@property CGFloat contentsScale;
@end
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1090
enum {
  NSPaperOrientationPortrait	= NSPortraitOrientation,
  NSPaperOrientationLandscape	= NSLandscapeOrientation
};
typedef NSInteger NSPaperOrientation;
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 101000
@interface NSWorkspace (AvailableOn101000AndLater)
- (BOOL)accessibilityDisplayShouldIncreaseContrast;
- (BOOL)accessibilityDisplayShouldDifferentiateWithoutColor;
- (BOOL)accessibilityDisplayShouldReduceTransparency;
@end
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1090
@interface NSView (AvailableOn1090AndLater)
- (void)setLayerUsesCoreImageFilters:(BOOL)usesFilters;
@end
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 101200
enum {
  NSCompositingOperationSourceOver = NSCompositeSourceOver
};
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 101200
enum {
  NSControlSizeRegular	= NSRegularControlSize,
  NSControlSizeSmall	= NSSmallControlSize
};
#endif

#if MAC_OS_X_VERSION_MAX_ALLOWED < 101100
typedef NSInteger NSGlyphProperty;

@interface NSLayoutManager (AvailableOn101100AndLater)
- (NSUInteger)getGlyphsInRange:(NSRange)glyphRange glyphs:(CGGlyph *)glyphBuffer
		    properties:(NSGlyphProperty *)props
	      characterIndexes:(NSUInteger *)charIndexBuffer
		    bidiLevels:(unsigned char *)bidiLevelBuffer;
@end
#endif
