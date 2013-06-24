#!/bin/bash
# xinitrc

# if want to use slim, then
# edit /etc/slim.conf
# 1. change login_cmd to exec/bin/bash -login ~/.xinitrc %session
# 2. comment sessiondir
# 3. set sessions to awesome,gnome

# if want to autologin, without xdm, then
# 1. set up a rc.local file
# 2. its content is like this " su - <username> -c startx"

# Merge Xresources file
xrdb -merge ~/.Xresources

# disable screensaver/screen blanking
xset s off
xset dpms 0 0 0

#fcitx
export XMODIFIERS="@im=fcitx"
export QT_IM_MODULE=fcitx
export GTK_IM_MODULE=fcitx
fcitx &

# dropbox
~/.dropbox-dist/dropboxd &

# battery alarm
~/.battery_alarm.sh &

#awesome
exec awesome -c ~/.config/awesome/rc.lua
