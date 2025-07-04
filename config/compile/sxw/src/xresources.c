#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xresource.h>

#include "xresources.h"

const char* colors[SchemeLast][2] = {
    [SchemeRed]        = { "#fb4f33", "#222222" },
    [SchemeGreen]      = { "#b8bb26", "#222222" },
    [SchemeYellow]     = { "#fabd2f", "#222222" },
    [SchemeBlue]       = { "#83a598", "#222222" },
    [SchemePurple]     = { "#d3869b", "#222222" },
    [SchemeAqua]       = { "#8ec07c", "#222222" },
    [SchemeLightGrey]  = { "#ffffff", "#444444" }
};

char *xresloadcolor(Display *dpy, const char *resource) {
    if (!dpy) return NULL;

    XrmInitialize();
    char *xrm_str = XResourceManagerString(dpy);
    if (!xrm_str) return NULL;

    XrmDatabase xrdb = XrmGetStringDatabase(xrm_str);
    if (!xrdb) return NULL;

    XrmValue value;
    char *type;

    if (XrmGetResource(xrdb, resource, NULL, &type, &value) == True) {
        if (value.addr != NULL && strnlen(value.addr, 8) == 7 && value.addr[0] == '#') {
            for (int i = 1; i <= 6; i++) {
                if ((value.addr[i] < '0') || 
                    (value.addr[i] > '9' && value.addr[i] < 'A') || 
                    (value.addr[i] > 'F' && value.addr[i] < 'a') || 
                    (value.addr[i] > 'f')) {
                    return NULL;
                }
            }
            char *color = malloc(8);
            if (color) {
                strncpy(color, value.addr, 7);
                color[7] = '\0';
            }
            return color;
        }
    }
    return NULL;
}

void load_colors_from_Xresources(Display *dpy) {
	XrmDatabase db;
    XrmValue value;
    char *type;
    char *resource_manager_string;

    resource_manager_string = XResourceManagerString(dpy);
    if (!resource_manager_string) {
        fprintf(stderr, "Failed to get X resource manager string\n");
        return;
    }

    // Convert the resource manager string into a resource database
    db = XrmGetStringDatabase(resource_manager_string);
    if (!db) {
        fprintf(stderr, "Failed to parse X resources\n");
        return;
    }

    for (int i = 0; i < SchemeLast; i++) {
        if (colors[i][1] != NULL && colors[i][1][0] != '#' && colors[i][1][0] != '\0') {
            free(colors[i][1]);
        }

		char name[16];
        snprintf(name, sizeof(name), "color%d", i);
        if (XrmGetResource(db, name, "*", &type, &value)) {
            // The value.addr holds the color value (e.g., "#282c34")
            colors[i][0] = value.addr;  // Use this for the foreground color
        } else {
            colors[i][0] = "#000000";  // Default to black if not found
        }

        if (strcmp(colors[i][1], "#222222") == 0) {
            char *color0 = xresloadcolor(dpy, "color0");
            if (color0) {
                colors[i][1] = color0;
            }
        } else if (strcmp(colors[i][1], "#444444") == 0) {
            char *color8 = xresloadcolor(dpy, "color8");
            if (color8) {
                colors[i][1] = color8;
            }
        }
    }
}
