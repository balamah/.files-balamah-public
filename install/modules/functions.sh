#!/bin/bash

sourceConfigRoot() {
	user=$(cat /tmp/.files-balamah-user)

	. /home/$user/.files-balamah/settings.conf
}


# dumb shit, but i don't know how to make it better
[ "$USER" = 'root' ] && sourceConfigRoot || . ~/.files-balamah/settings.conf 

rmodd() {
	sed "s/#.*//;/^$/d"	
}

getPackageManager() {
	case "$distro" in
		"arch")   echo 'pacman -S --noconfirm' ;;
		"debian") echo 'apt-get install -y'    ;;
		"gentoo") echo 'emerge --quiet'        ;;
	esac
}

link() {
	[ -z "$1" ] || [ -z "$2" ] && echo '$@ - from. last - to' && exit 1

	to="${!#}"
	from="$@"

	printf '=%.0s' {1..20} && echo " Linking to $to"

	echo "$from" | sed 's/ /\n/g' | head -n -1 | \
		while read file; do
			ln -s "$file" "$to" 2> /dev/null && \
				echo "$file --> $to" || \
				echo "$file !->! $to"
		done
}

flatpakAdd() {
	# $1 :: program name with .
	# $2 :: catalog name

	sudo flatpak override "$1" --filesystem="$2" && \
		echo "$1 added rw --> $2" || echo "$1 didn't add rw !-->! $2"
}

flatpakRemove() {
	# $1 :: program name with .
	# $2 :: catalog name

	sudo flatpak override --reset "$1" --filesystem="$2" && \
		echo "$1 removed rw --> $2" || echo "$1 didn't remove rw !-->! $2"
}

flatpakPermission() {
	# $1 :: program name with .
	# $2 :: catalog name
	# $3 :: reset (use --reset), optional
	# If catalog name begins doesn't begin with /,
	# it will determine that directory is located in home

	[ -z "$1" ] || [ -z "$2" ] && return 1
	[ "${2:0:1}" != '/' ] && directory="$HOME/" || directory=''

	catalogPath="$directory$2"

    [ "$3" = '--reset' ] && \
		flatpakRemove "$program" "$catalogPath" || flatpakAdd "$program" "$catalogPath"
}

