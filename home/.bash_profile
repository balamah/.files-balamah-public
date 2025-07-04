. ~/.files-balamah/config/scripts/functions.sh
. ~/.files-balamah/settings.conf

export XDG_DESKTOP_DIR="$HOME"
export QT_QPA_PLATFORMTHEME=qt5ct

wmStartup() {
	clear
	neofetch
	colorScript "already"

	printf "$profile is going to be launched"

	startx
}

[ "$(tty)" = '/dev/tty1' ] && wmStartup
