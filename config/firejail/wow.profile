# -*- mode: conf-space -*-

# include /etc/firejail/wine.profile
include whitelist-var-common.inc

# Or use a full private home (like you're doing)
private ~/.firejail/

# Allow game folder access
whitelist /mnt/gutter/wow

# Make input working
whitelist /dev/input/event*
whitelist /dev/uinput
whitelist /dev/dri/card0
whitelist /dev/dri/renderD128
whitelist /dev/shm

# Improve security
private-dev
private-tmp
caps.drop all
seccomp
nonewprivs
noroot
seccomp
