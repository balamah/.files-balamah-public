/*
 * Modify this file to change what commands output to your statusbar,
 * and recompile using the make command.
 * There are 4 parameters for each block:
 * - Icon
 * - Command
 * - Update interval (in seconds)
 * - Update signal
 */

/* First block should have signal 0, for it won't look ugly */
static const Block blocks[] = {
	{" ", "~/.config/scripts/get/cpu-load",      1,   1},
	{" ",  "~/.config/scripts/get/ram-usage",    1,   2},
	{" ",  "~/.config/scripts/get/uptime",        4,   3},
	{"",    "~/.config/scripts/get/volume",        1,   4},
	{"󰌌 ", "~/.config/scripts/get/layout/layout", 0,   6},
	{"",     "~/.config/scripts/get/weather",      300, 5},
	{" ",  "~/.config/scripts/get/time",          1,   8},
	{" ",  "~/.config/scripts/get/date",          1,   7}
};

/*
 * Sets delimiter between status commands.
 * NULL character ('\0') means no delimeter.
 * Delimeter should be in format " D", otherwise it looks ugly
 */
static char delim[] = "  ";
static unsigned int delimLen = 5;
