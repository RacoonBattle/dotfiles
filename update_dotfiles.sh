#!/bin/bash
# This script will apply my ~/dotfiles/ config files to ~/
#	1. creates symlinks from the home directory to any desired dotfiles 
#	2. some config should be different ( eg: irc and mail config in laptop and company)
#	   so update these config and apply relevant patch.

# mkdir
mkdir -p ~/Pictures/Shot
mkdir -p ~/.vim/backup
mkdir -p ~/.config/fcitx/skin
mkdir -p ~/.config/gtk-3.0

# link files
link_conf_files=".bash_profile .gitconfig .gtkrc-2.0 .gvimrc 
.screenrc .vimperatorrc .vimrc .xinitrc .xmodmaprc .Xresources
.config/gtk-3.0/settings.ini
.config/user-dirs.dirs
.config/ranger/rifle.conf
.config/awesome/calendar2.lua
.config/awesome/rc.lua
.config/fcitx/config
.config/fcitx/conf/fcitx-cloudpinyin.config
.config/fcitx/conf/fcitx-classic-ui.config"

for i in $link_conf_files; do
	rm -rf ~/$i
	cp -rs ~/dotfiles/$i ~/$i
	ls -l --color=auto ~/$i
done
echo "create symlinks done"
echo 

# copy files
copy_conf_files=".bashrc
.config/awesome/theme
.config/fcitx/profile
.config/fcitx/skin/anran"
for i in $copy_conf_files; do
	rm -rf ~/$i
	cp -rf ~/dotfiles/$i ~/$i
	echo "overwrite $i"
done

# apply patch
if [ $HOSTNAME = optiplex-760 ];then
	echo "$HOSTNAME is company's workstation"
	sh ~/Dropbox/company_rc.sh
	echo "apply company's patch done"
fi

