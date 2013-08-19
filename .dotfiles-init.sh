#!/bin/bash

# make the $HOME as dotfiles repo dir
if [ -d $HOME/dotfiles ]; then
	orig_repo_dir="$HOME/dotfiles"
	cd ~ && mv $orig_repo_dir/.git . && rm -rf $orig_repo_dir && git reset --hard
fi

# awesome wm
git clone http://git.sysphere.org/vicious ~/.config/awesome/vicious

# vim
git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
mkdir ~/.vim/backup
vim +BundleInstall +qa

