#!/bin/bash

set -e

. ~/.files-balamah/settings.conf

[ ! -f ~/.files-balamah/python-scripts.ini ] && echo 'no python-scripts.ini' && exit 1

# Dumb shit, but i don't know how to use functions with root
# priveleges and user paths at the same time
echo "$USER" > /tmp/.files-balamah-user

# You may say that i made everything complicated by splitting
# one script into many files.
# You may be right, but if something was deleted or broken,
# it can be easily fixed by just launching module instead pasting
# certain lines from install.sh into other file
modules/package-install
modules/profile-packages-install 
modules/enable-services
modules/flatpak
modules/link
sudo modules/system-link 
modules/additional-commands
modules/compile-programs
modules/create-home

# Custom installation script.
# Configure variables below to use custom installation script
# - $enableCustomInstallationScript
# - $customInstallationScriptPath
[ "$enableCustomInstallationScript" = 'yes' ] && "$customInstallationScriptPath"
