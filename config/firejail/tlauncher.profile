# -*- mode: conf-space -*-

# Firejail profile for tlauncher
# Persistent global definitions
include globals.local

# Allow java (blacklisted by disable-devel.inc)
include allow-java.inc

# make tlauncher and minecraft work
whitelist ${HOME}/.firejail/.minecraft
whitelist ${HOME}/.firejail/.tlauncher

# isolate
private ~/.firejail

# some other settings
caps.drop all
machine-id
nodvd
nogroups
notv
nou2f
tracelog

# private-etc alternatives,fonts,passwd - minimal required to run but will probably break
# program!
private-dev
private-tmp
