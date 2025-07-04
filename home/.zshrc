#!/bin/zsh

# source some configs
source ~/.config/scripts/functions.sh
source ~/.files-balamah/settings.conf

# Variables creation
export EDITOR=nvim
export TERM='xterm-256color'
export DATE=$(date +"%d%m%Y")

bindkey -e

getDistro() {
	cat /etc/os-release | grep "^NAME" | sed 's/.*=//g;s/"//g'
}

[ "$(getDistro)" = 'Arch Linux' ] && export VARPACMAN=/var/cache/pacman/pkg/

# PATH confguration
export PATH="$PATH:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/.config/emacs/profiles/doom/bin:$JAVA_HOME"

# java configuration
export JAVA_HOME=/usr/lib/jvm/java-21-openjdk

# change term title to pwd
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%~\a" }

# tab completion highlight
autoload -Uz compinit
compinit
zstyle ':completion:*' menu select

# better completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

# delete annoying dogshit when you want to delete word.
# It deleted whole path for some reason
autoload -U select-word-style
select-word-style bash

# C-l to clear terminal without scrollback.
# I want to clear the terminal, i want to CLEAR
# without scrollback
function clear-scrollback-buffer {
  clear && printf '\e[3J'
  zle && zle .reset-prompt && zle -R
}

zle -N clear-scrollback-buffer
bindkey '^L' clear-scrollback-buffer

# history configuration
HISTFILE=~/.cache/zhistory
HISTSIZE=100000
SAVEHIST=100000
setopt appendhistory

# bookmarks
jump() {
    local directory
	local file
	local bookmarks

	file=cli-bookmarks.dmenu-ignore
	[ ! -f "$bookmarksPath"/"$file" ] && \
		bookmarks="$standardBookmarksPath"/"$file" || \
		bookmarks="$bookmarksPath"/"$file"	

    directory=$(br $bookmarks | sort | fzf | awk '{print $2}' | sed "s@~@$HOME@g")

    [ -z "$directory" ] && return 0

    builtin cd "$directory" && echo && lsd && echo "you are now in $directory"

    zle reset-prompt
}

edit() {
	file=cli-bookmarks.dmenu-ignore
	[ ! -f "$bookmarksPath/$file" ] && \
		bookmarks="$standardBookmarksPath/$file" || \
		bookmarks="$bookmarksPath/$file"	

    $EDITOR "$bookmarks"
}

zle -N jump && zle -N edit

bindkey "^[n" jump
bindkey "^[q" edit

# POWERLEVEL config
POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true

instantPrompt="${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" 
[[ -r "$instantPrompt" ]] && source "$instantPrompt"

# apply theme
[ ! -f ~/.is-sandbox ] && \
	source ~/.config/shell/zsh/theme-config/00-p10k-base.zsh || \
	source ~/.config/shell/zsh/theme-config/10-p10k-sandbox.zsh

# color script
colorScript "no"
zle -N colorScript
bindkey '^[r' colorScript

# reload zshrc
function zshrcReload {
	source ~/.zshrc
}

zle -N zshrcReload
bindkey '^[i' zshrcReload

# Plugins
### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

zinit load romkatv/powerlevel10k
zinit load zsh-users/zsh-syntax-highlighting
zinit load zsh-users/zsh-autosuggestions
zinit load hlissner/zsh-autopair

### End of Zinit's installer chunk
### End of Zinit's installer chunk

# tmux configuration
tmuxStart() {
	if ! tmux has-session -t default 2>/dev/null; then
        tmux new-session -s default
        tmux attach-session -t default
    else
        tmux attach-session -t default
    fi
}


[ -z "$TMUX" ] && [ "$enableAutoTmux" = 'yes' ] && tmuxStart || echo 2> /dev/null

## Unbind this dogshit
## Couldn't unbind in config file unfortunately
[ ! -z "$TMUX" ] && tmux unbind-key -T root C-k || echo 2> /dev/null 

# apply some aliases
source ~/.config/shell/aliases.sh

[[ -f /tmp/.files-balamah/aliases.zsh ]] && source /tmp/.files-balamah/aliases.zsh
[[ -f ~/.config/shell/local-aliases.zsh ]] && source ~/.config/shell/local-aliases.zsh
