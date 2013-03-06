#!/bin/bash
# This script will apply my ~/dotfiles/ config files to ~/
#	1. creates symlinks from the home directory to any desired dotfiles 
#	2. some config should be different ( eg: irc and mail config in laptop and company)
#	   so update these config and apply relevant patch.

# link files
home_conf_files=".bash_profile .gitconfig .gtkrc-2.0 .gvimrc 
.screenrc .vimperatorrc .vimrc .xinitrc .xmodmaprc .Xresources
.config/user-dirs.dirs
.config/ranger/rifle.conf
.config/awesome/calendar2.lua
.config/awesome/rc.lua
.config/awesome/theme
.config/fcitx/config
.config/fcitx/profile
.config/fcitx/skin/dark/fcitx_skin.conf"

mkdir -p ~/.config/fcitx/skin/dark/

for i in $home_conf_files; do
	rm -rf ~/$i
	cp -rs ~/dotfiles/$i ~/$i
	ls -l --color=auto ~/$i
done
echo "create symlinks done"
echo 

# copy files
cp ~/dotfiles/.bashrc ~/.bashrc

# apply patch
if [ $HOSTNAME = optiplex-760 ];then
	echo "$HOSTNAME is company's workstation"
	sh ~/Dropbox/company_rc.sh
	echo "apply company's patch done"
fi

