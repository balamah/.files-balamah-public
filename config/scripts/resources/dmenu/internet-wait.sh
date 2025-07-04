#!/bin/sh

. ~/.files-balamah/settings.conf 

while true; do
	ping -q -c1 "$connectionTestWebsite" && \
		notify-send "${0##*/}" 'internet is here' -u critical && exit
done
