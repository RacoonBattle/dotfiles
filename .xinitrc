#!/bin/bash
# xinitrc
# in this way you have to edit /etc/slim.conf
# 1. change login_cmd to exec/bin/bash -login ~/.xinitrc %session
# 2. comment sessiondir
# 3. set sessions to awesome,gnome

# Merge Xresources file
xrdb -merge ~/.Xresources

#fcitx
export XMODIFIERS="@im=fcitx"
export QT_IM_MODULE=xim
export GTK_IM_MODULE=xim
fcitx &

#awesome
exec awesome -c ~/.config/awesome/rc.lua
