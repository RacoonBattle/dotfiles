#!/bin/bash
# This script will apply my ~/dotfiles/ config files to ~/
#	1. creates symlinks from the home directory to any desired dotfiles 
#	2. some config should be different ( eg: irc and mail config in laptop and company)
#	   so update these config and apply relevant patch.

# link_files
home_conf_files=".bashrc .bash_profile .gitconfig .gtcrc-2.0 .gvimrc 
.screenrc .vimperatorrc .vimrc .xinitrc .xmodmaprc .Xresources
.config/user-dirs.dirs
.config/ranger/rifle.conf
.config/awesome/calendar2.lua
.config/awesome/rc.lua
.config/awesome/theme"

for i in $home_conf_files; do
	rm -rf ~/$i
	ln -s ~/dotfiles/$i ~/$i
	ls -l --color=auto ~/$i
done
echo "create symlinks done"

