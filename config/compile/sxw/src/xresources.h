#ifndef XRESOURCES_H
#define XRESOURCES_H

#include <X11/Xlib.h>
#include <X11/Xresource.h>

enum { SchemeRed,
	   SchemeGreen,
	   SchemeYellow,
	   SchemeBlue,
	   SchemePurple,
	   SchemeAqua,
	   SchemeLightGrey,
	   SchemeLast
};

extern const char* colors[SchemeLast][2];

char *xresloadcolor(Display *dpy, const char *resource);
void load_colors_from_Xresources(Display *dpy);

#endif // XRESOURCES_H
