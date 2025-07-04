#!/bin/bash

. ~/.files-balamah/settings.conf 

# It's script with functions that help to avoid repetition

rip() {
	# Rips spaces from beginning and end of the string.
	# It looks ugly when you open recents and you see spaces around
	
	sed 's/^ //g;s/ $//g'
}

toggle() {
	# Toggle values.
	# The chain of toggle looks like 1 -> 2 -> 3 -> 1 -> ... .
	# Every time it switches to the next element,
	# and if element is last, that it switches to the first.
	# $1 :: list (using ,). Like "1,2,3,4,5"
	# $2 :: storage file

	[ -z "$1" ] || [ -z "$2" ] && echo '$1 - list ("1,2,3,4,5"). $2 - storage file' && \
		return 1

	list="$1"
	file="$2"

	IFS=',' read -r -a array <<< "$list"
	data=$(cat "$2" || echo "${array[0]}")
	dataIndex=$(echo ${array[@]/$data//} | cut -d/ -f1 | wc -w)
	
	[ "$data" = "${array[-1]}" ] && \
		newData="${array[0]}" || \
		newData=${array[dataIndex+1]}

	echo "$newData" | tee "$file"
}

ttoggle() {
	# Toggle 2 values, and second variable can be changed.
	# First element should be constant, and other can be changed,
	# if element is not equal to the first, it switches to the first.
	# It works like: "first element -> ? -> first element -> ...".
	# $1 :: list (using ,)
	# $2 :: storage file

	[ -z "$1" ] || [ -z "$2" ] && echo '$1 - list ("1,2"). $2 - storage file' && \
		return 1

	list="$1"
	file="$2"

	IFS=',' read -r -a array <<< "$list"
	data=$(cat "$2" || echo "${array[0]}")
	dataIndex=$(echo ${array[@]/$data//} | cut -d/ -f1 | wc -w)

	first="${array[0]}"
	second="${array[1]}"

	[ "$data" = "$first" ] && newData="$second" || newData="$first"

	echo "$newData" | tee "$file"
}

includeFiles() {
	# Used for function br to include bookmark files
	# It's recursion. br {include -> br}

	files=$(cat "$1" | grep '^!INCLUDE:' | sed "s@~@$HOME@g;s/.*: //" | rip)

	[ -z "$files" ] && return 1

	echo "$files" | \
		while read file; do
			br "$file"
		done
}

fl() {
	# Get first letters for key in bookmark ALL expression.
	# Which is Format Line
	
	awk -F "$1" '{for(i=1;i<=NF;i++) printf "%s", substr($i,1,1)}'
}

bfl() {
	# Format lines for ALL expression.
	# Which is Bookmark Format Lines
	
	while IFS= read -r line; do
		directory=$(dirname "$line")
		file="${line##*/}"

		# Looks dumb, but it was made this way to make changable bookmark prefix
		directoryFirstLetters=$(echo "$directory" | fl '/' | \
									tr '[:upper:]' '[:lower:]' | \
							        xargs -I "%" echo "%/")

		fileFirstLetters=$(echo "$file" | sed -E 's/^[0-9]{8}-//g' | fl '-')

		[ ! -z "$1" ] && directoryFirstLetters="$1"
		[ "$1" = '^$' ] && directoryFirstLetters=""

		key="$directoryFirstLetters$fileFirstLetters"
		value=$(echo "$line" | sed 's/ /\\\ /g')

		printf "%-20s %s\n" "$key" "$value" | sed "s@^\./@@;s@^/@@;s/^h.//"
	done
}

bfad() {
	# Shortened from Bookmark Format ALL Directory.
	# Find the files with pattern and formats to bookmark

	[ -z "$1" ] || [ -z "$2" ] && \
		echo '$1 - directory. $2 - type (d f l). $3 - pattern' && return 1

	directory=$(echo "$1" | sed "s@^\~@$HOME@g")
	type="$2"
	pattern="$3"
	prefix="$4"
	
	[ -z "$pattern" ] && find "$directory" -type "$type" | sort | bfl "$prefix" || \
						 find "$directory" -name "$pattern" -type "$type" | sort | \
							 bfl "$prefix"
}

getColumn() {
	# Simplified awk.
	# $1 :: string
	# $2 :: column number
	# $3 :: separator (optional)

	echo "$1" | awk -F "$3" "{print \$$2}" | rip
}

readALL() {
	# Make ALL expression working
	
	grep '^!ALL:' "$1" | \
		while read string; do
			params=$(echo "$string" | awk -F ":" '{print $2}')

			directory=$(getColumn "$params" 2 "|")
			type=$(getColumn "$params" 3 "|")
			pattern=$(getColumn "$params" 4 "|")
			prefix=$(getColumn "$params" 5 "|")

			bfad "$directory" "$type" "$pattern" "$prefix"
		done
}

br() {
	# Shortened from Bookmark Read.
	# Deletes comments and empty lines, includes bookmark files and
	# formats ALL expressions

	[ -z "$1" ] && echo '$1 - bookmark file' && return 1

	cat "$1" | sed "s/#.*//;/^!/d;/^$/d"
	includeFiles "$1"

	[ "$enableBookmarksALLexpression" = 'yes' ] && readALL "$1"
}

colorScript() {
	# Run random color script from ~/.config/scripts/additional/color-scripts.
	# $1 :: don't clear the screen

	[ -z "$1" ] && clear

	find ~/.config/scripts/additional/color-scripts \
		-type f -executable | shuf -n 1 | xargs bash

	zle .reset-prompt 2> /dev/null
	printf '\n'
}

isPywalEnabled() {
	[ "$enablePywal" = 'yes' ] && return 0 || return 1
}

isPywalColorscheme() {
	[ "$colorscheme" = 'pywal' ] && return 0 || return 1
}

printParameters() {
	cat "$0" | grep -E '^#\s*\$[0-9@#*!{}].*::' | sed 's/^# //'
}
