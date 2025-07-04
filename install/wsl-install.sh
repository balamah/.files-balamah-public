#!/bin/bash

set -e

. ~/.files-balamah/settings.conf

[ ! -f ~/.files-balamah/python-scripts.ini ] && echo 'no python-scripts.ini' && exit 1

# You may say, i made everything complicated by splitting
# one script into many files.
# You may be right, but if something was deleted or broken,
# it can be easily fixed by just launching file instead pasting
# certain lines into other file
modules/package-install --wsl
modules/link
modules/additional-commands
modules/zsh-configuration
modules/create-home
