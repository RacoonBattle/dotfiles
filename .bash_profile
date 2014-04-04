# .bash_profile

# source .bashrc
if [ -f ~/.bashrc ]
then
	. ~/.bashrc
fi

# daemon
if [ "$(tty)" = "/dev/tty1" ]; then
	mpd &
fi
