#!/bin/sh

. ~/.files-balamah/settings.conf 

# files
alias ls='lsd -F --group-directories-first'
alias sl='ls'
alias lh='ls -lh'
alias ll='ls -lhA'
alias lla='ls -lha'
alias la='ls -a'
alias l.='ls -a -d .*'
alias lh.='ls -lah -d .*'
alias tree='lsd --tree'
alias organize='python3 ~/.config/scripts/python/fileorganizer.py'
alias exp='explorer.exe .' # useful for wsl

# python
alias python='python3'
alias py='python3'
alias pym='py -m'
alias pywal='python3 -m pywal'

# editors
alias v='nvim'
alias sv='sudo nvim'
alias ed='ed -p ":"'
alias emacs='emacs --debug-init'
alias em='emacsclient -s "default" -t'

# navigation
alias cd..='cd ..'
alias ..='cd..'
alias ...='cd ../..'
alias .3='cd ../../../'
alias .4='cd ../../../../'
alias .5='cd ../../../../../'
alias rn='ranger'

# convenience
alias cd.='cd'
alias start='xdg-open'
alias smake='sudo make && sudo make install'
alias xcopy='xclip -selection clipboard -r'
alias xpaste='xclip -o -selection clipboard'
alias suod='sudo'
alias mkvenv='virtualenv venv/'
alias killterm='cat "/proc/$PPID/comm" | xargs -I "%" killall "%"'
alias firejail-home='firejail --private=/home/balamah/firejail'
alias wal-telegram='~/.config/scripts/additional/wal-telegram/wal-telegram'
alias :wq='exit'
alias update-cron='sudo cat ~/.files-balamah/cron/crontab > /var/spool/cron/$USER'
alias git-untracked='git ls-files . --exclude-standard --others'
alias git-branch='git rev-parse --abbrev-ref HEAD'
alias grep='grep --color'
alias wfirejail='firejail --profile=wine'
alias fucking='sudo'
alias uchown="sudo chown -R $USER:$USER ."
alias neonazi="neofetch"

# fix dumb shit when you copy the code.
# Some devs like to put $ on the beginning of commands,
# and you need to delete each "$" because it looks better than
# just writing: "run as user" or "run as root"
alias "$"=';'

# functions

function fspc () {
	sed 's/ /\\\ /g;s/[[|]]/&\\ /g;s/[(|)]/&\\ /g'
}

function cdfind () {
	find "$@" | fspc
}

function suckless-patch () {
	xpaste | sed 's@config\.def\.h@config.h@g' | patch -p1
}

function bible-clean () {
	awk '{for (i=1; i<=NF; i++)
			if ($i ~ /^[0-9]+:[0-9]+$/) {
				for (j=i+1; j<=NF; j++)
					printf "%s%s", $j, (j<NF?OFS:"\n");
				break}}'
}

function copy-minecraft-config () {
	[ -z "$1" ] && echo 'you need to enter version' && return 1

	storage="$minecraftConfigsPath/$1"
	[ ! -d "$storage" ] && mkdir "$storage"

	cp ~/.minecraft/options.txt $storage
	cp ~/.minecraft/optionsof.txt $storage 2> /dev/null
}

function docker-php () {
	docker exec -it $(docker ps -a | grep php | awk '{print $1}') /bin/bash
}

# function docker-symfony () {
# 	id=$(docker ps -a | grep symfony | awk '{print $1}' | shuf -n 1)

# 	[ -z "$1" ] && \
# 		docker exec -it $id /bin/bash ||
# 		docker exec -it $id /bin/bash -c "$@" ||
# }

function docker-symfony () {
    id=$(docker ps -a | grep "symfony.*Up" | awk '{print $1}' | shuf -n 1)

    [ -z "$1" ] && \
        docker exec -it "$id" /bin/bash || \
        docker exec -it "$id" /bin/bash -c "$*"
}

function root () {
	[ -z "$1" ] && sudo -E zsh && return

	sudo -E "$1"
}

function mkcd () {
	mkdir $1; cd $1/
}

function scc () {
	# Shortened from script create.
	# Creates executable shell script. Also can make script from file
	# $1 :: file name

	[ -z "$1" ] && echo '$1 should be file name' && return 1

	[ ! -f "$1" ] && touch "$1"

	chmod +x "$1"

	[ -z "$2" ] && printf '#!/bin/sh\n\n\n' > "$1" || \
		printf "#!/bin/$2\n\n\n" > "$1"
}

function git-commit-push () {
	# $1 :: commit message

	[ -z "$1" ] && echo '\$1 - commit message' && return 1

	git add .
	[ "$1" != '--edit' ] && git commit -m "$1" || git commit
	
	git-branch | xargs git push -u origin
}

function cdrm () {
	catalogName="${PWD##*/}"
	[ -z "$catalogName" ] && return 0
	cd .. && rm -r "$catalogName"
}

function docker-rm-containers () {
    docker ps -a -q | xargs -r docker stop
    docker ps -a -q | xargs -r docker rm
    docker image ls -q | xargs -r docker image rm --force
    docker volume ls -q | xargs -r docker volume rm
    docker system prune --all
}

function lorg () {
	[ -z "$1" ] && echo '$1 should be leetcode problem from url' && return 1

    python3 ~/.config/scripts/python/leetcode-org-converter/main.py "$1"
}

function replace-files () {
    for file in *; do
		mv -- "$file" "${file%.*}.$1";
	done
}

function replace-extension () {
    for file in *.$1; do
		mv -- "$file" "${file%.*}.$2";
	done
}

function cd () {
    builtin cd "$@" && ls
}

function extr () {
	[ ! -f "$1" ] && echo '$1 is not valid to extract'

	case $1 in
		*.tar.bz2) tar xjf "$1" ;;
		*.tar.gz)  tar xzf "$1" ;;
		*.tar.xz)  tar xf  "$1" ;;
		*.tar.zst) tar xf  "$1" ;;
		*.tar)     tar xf  "$1" ;;
		*.tbz2)	   tar xjf "$1" ;;
		*.tgz)	   tar xzf "$1" ;;
		*.gz)	   gunzip  "$1" ;;
		*.zip)	   unzip   "$1" -d "extracted-$1" ;;
	esac
}

function mpv-lconf () {
	# It launches series with config.
	# For example, you want to play some series, but you 
	# need to manually import video and audio road. 
	# The function automates this
	
	[ -z "$1" ] && echo '$1 should be episode number' && return
	
	episode="$1"
	source launch.conf

	mpv "$video" --audio-file="$audio"
}

function mbible () {
	book="$1"
	shift

	for verse in "$@"; do
		bible "$book" "$verse"
	done | cat
}

function cbible () {
	[ -z "$1" ] && echo '$1 - book name
$2 - verses (1cor 15:20-21,23-24,26)' && return 1

	book="$1"
	chapter="${2%%:*}"

	echo "${2##*:}" | sed 's/,/\n/g' | \
		while read verse; do
			bible "$book $chapter:$verse"
		done | cat
}

function sedf () {
	# sed find.
	# $1 :: pattern to find recursively
	# $2 :: substitution for searched pattern
	# $3 :: (optional) symbol for sed separation (@, #)

	[ -z "$1" ] || [ -z "$2" ] && \
		echo '$1 - pattern to find, $2 - substitution for searched pattern' && return 1

	[ -z "$3" ] && symbol="/" || symbol="$3"

	grep -Rl "$1" | xargs -I "%" sed -i "s$symbol$1$symbol$2$symbol g" "%"
}
