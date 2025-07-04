#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <X11/Xlib.h>
#include <X11/Xft/Xft.h>
#include <unistd.h>
#include <signal.h>

#include "drw.h"
#include "util.h"
#include "config.h"

#define LENGTH(X) (sizeof X / sizeof X[0])
#define TEXTW(X) (drw_fontset_getwidth(drw, (X)))


static Display* dpy;
static int scr;
static Window root;
static Window win;
static XSetWindowAttributes xwa;

static Drw* drw;
static Clr* scheme[SchemeLast];

#define UPDATE_TIME 300 /* in seconds */

static int x_pos;
static int y_pos;

static int WIDTH;
static int HEIGHT;

static int textsize;

static Fnt* big;
static Fnt* small;

static void redraw()
{
    /* clear screen of previous contents */
    drw_rect(drw, 0, 0, WIDTH, HEIGHT, 1, 1);

    /* put whatever you want to draw here */

    char output[32];
    sh("scripts/planets.sh | tr -s ' '", output, 32);

    char* event;
    char* time;
    char* delimiter = "$";

    event = strtok(output, delimiter);
    time = strtok(NULL, delimiter);

    drw_setfontset(drw, big);

    int lpad = (WIDTH - TEXTW(event)) / 2;
    drw_text(drw, lpad, 0, WIDTH, (int)(HEIGHT*0.6667), 0, event, 0);

    drw_setfontset(drw, small);
    int timewidth = TEXTW(time);
    drw_text(drw, lpad, (int)(HEIGHT*0.4667), timewidth, (int)(HEIGHT*0.3333), 0, time, 0);
		
    /* you should only be changing this part and the WIDTH, HEIGHT and UPDATE_TIME */

    /* put the drawn things onto the window */
    drw_map(drw, win, 0, 0, WIDTH, HEIGHT);

}

void sigalrm(int signum)
{
    XEvent exppp;

    memset(&exppp, 0, sizeof(exppp));
    exppp.type = Expose;
    exppp.xexpose.window = win;
    XSendEvent(dpy,win,False,ExposureMask,&exppp);
    XFlush(dpy);

    alarm(UPDATE_TIME);
}

int
main(int argc, char** argv)
{

    /* if x and y pos are not provided, then throw error */
    if(argc != 7)
	die("usage: planets [xpos] [ypos] [width] [height] [override-redirect] [text-size]");
	
    /* convert string arguments to integers, argv[0] is the command itself */
    x_pos = atoi(argv[1]);
    y_pos = atoi(argv[2]);

    WIDTH = atoi(argv[3]);
    HEIGHT = atoi(argv[4]);

    int override_redirect = atoi(argv[5]);
	
    textsize = atoi(argv[6]);

    /* set alarm signal to update the widget contents */
    signal(SIGALRM, sigalrm);

    /* obtain connection to X server */
    dpy = XOpenDisplay(NULL);
    if(dpy == NULL)
	die("Could not open display.");

    /* create window */
    scr = DefaultScreen(dpy);
    root = RootWindow(dpy, scr);

    xwa.background_pixel = BlackPixel(dpy, scr);
    xwa.override_redirect = override_redirect; /* This will tell the WM to ignore this window */
    xwa.backing_store = WhenMapped;

    win = XCreateWindow(dpy, root, x_pos, y_pos, WIDTH, HEIGHT, 0, DefaultDepth(dpy, scr), InputOutput, DefaultVisual(dpy, scr), CWBackPixel | CWOverrideRedirect, &xwa);


    /* tell X11 that we want to receive Expose events */
    XSelectInput(dpy, win, ExposureMask);

    /* set class hint so dwm does not tile */
    XClassHint* class_hint = XAllocClassHint();
	
    /* make SURE to change the res_name to the name of your widget */
    class_hint->res_name = "planets";
    class_hint->res_class = "widget";

    XSetClassHint(dpy, win, class_hint);


    /* put window on the screen */
    XMapWindow(dpy, win);

    /* create drw object to draw stuff */
    drw = drw_create(dpy, scr, root, WIDTH, HEIGHT);


    char big_font_buf[64];
    snprintf(big_font_buf, 64, "JetBrainsMonoNL NFM:size=%d", textsize);
    char small_font_buf[64];
    snprintf(small_font_buf, 64, "JetBrainsMonoNL NFM:size=%d", (int)(textsize * 0.6667));
    
    const char* font_big[] = {big_font_buf};
    const char* font_small[] = {small_font_buf};

    big = drw_fontset_create(drw, font_big, 1);
    small = drw_fontset_create(drw, font_small, 1);

    /* initialize color schemes from config.h */
    for (int i=0;i<SchemeLast;i++)
	scheme[i] = drw_scm_create(drw, colors[i], 2);

    drw_setscheme(drw, scheme[SchemeGreen]);
	

    /* start updater thread */
    alarm(UPDATE_TIME);
    redraw();

    /* main loop */
    XEvent ev;
    while(XNextEvent(dpy, &ev) == 0){
	switch(ev.type){
	case Expose:
	    redraw();
	}
    }

    /* close window and clean up */
    XUnmapWindow(dpy, win);
    XDestroyWindow(dpy, win);
    XCloseDisplay(dpy);

    return 0;
}
