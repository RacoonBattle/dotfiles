# .bashrc

# varialbes
export VISUAL=vim
export EDITOR=vim
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# history
export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
export HISTSIZE=1024
export HISTFILESIZE=10240
shopt -s histappend      # append to history, don't overwrite it
PROMPT_COMMAND="history -a"
			 # Save history after each command finishes
			 # please manually `history -n` to reload history file
			 # when want to get other tty's history command

# no chase link
set -P

# path
pathmunge()
{
	case ":${PATH}:" in
		*:"$1":*)
			;;
		*)
			if [ "$2" = "after" ]; then
				PATH=$PATH:$1
			else
				PATH=$1:$PATH
			fi
			;;
	esac
}

pathmunge /sbin:/usr/sbin after
pathmunge $HOME/bin after
pathmunge $HOME/scripts after
pathmunge $HOME/Dropbox/scripts after
pathmunge $HOME/Dropbox/work/scripts after
pathmunge /opt/android-sdk-update-manager/platform-tools/

export PATH


# completion
if [ -f /usr/share/bash-completion/bash_completion ]; then
	#fedora
	. /usr/share/bash-completion/bash_completion
fi
if [ -f /etc/profile.d/bash-completion.sh ]; then
	#gentoo
	. /etc/profile.d/bash-completion.sh
fi

#my alias
alias ga='git add -A'
alias gb='git branch'
alias gc='git commit -a'
alias gd='git diff'
alias gl='git log'
alias go='git checkout'
alias gs='git status'
alias gu='git pull --rebase'
alias gw='git show'

alias la='ls -lAh --color=auto'
alias ll='ls -lh --color=auto'
alias vi='vim'


# colorful multi lines bash prompt ----------
Color_Off='\e[0m'       # Text Reset

# Regular Colors
Black='\e[0;30m'        # Black
Red='\e[0;31m'          # Red
Green='\e[0;32m'        # Green
Yellow='\e[0;33m'       # Yellow
Blue='\e[0;34m'         # Blue
Purple='\e[0;35m'       # Purple
Cyan='\e[0;36m'         # Cyan
White='\e[0;37m'        # White

# Bold
BBlack='\e[1;30m'       # Black
BRed='\e[1;31m'         # Red
BGreen='\e[1;32m'       # Green
BYellow='\e[1;33m'      # Yellow
BBlue='\e[1;34m'        # Blue
BPurple='\e[1;35m'      # Purple
BCyan='\e[1;36m'        # Cyan
BWhite='\e[1;37m'       # White

# High Intensity
IBlack='\e[0;90m'       # Black
IRed='\e[0;91m'         # Red
IGreen='\e[0;92m'       # Green
IYellow='\e[0;93m'      # Yellow
IBlue='\e[0;94m'        # Blue
IPurple='\e[0;95m'      # Purple
ICyan='\e[0;96m'        # Cyan
IWhite='\e[0;97m'       # White

parse_git_branch()
{
	git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

# notes:
#	if use color, then PS1 should be PS1="\[$COLOR\]", otherwise wrap will go wrong
#	the basic multi lines should be PS1="\342\224\214\342\224\200\n\342\224\224\342\224\200\342\224\200>"

# multi line color
sq_color="\[$IBlue\]"

# xx files yy totle
DIR_INFO="\[$ICyan\]\$(/bin/ls -1 | /usr/bin/wc -l | /bin/sed 's: ::g') files\[$IYellow\] \$(/bin/ls -lah | /bin/grep -m 1 total | /bin/sed 's/total //')"

LINE1="\[$sq_color\][$DIR_INFO\[$sq_color\]]-\[$Red\]\$(parse_git_branch)\[$sq_color\][\[$IGreen\]\w\[$sq_color\]]"

LINE2="\[$IWhite\]\u\[$White\]@\[$IWhite\]\h\[$BBlue\] $ \[$Color_Off\]"

PS1="$sq_color\342\224\214\342\224\200$LINE1\n$sq_color\342\224\224\342\224\200\342\224\200>$LINE2"
PROMPT_COMMAND="$PROMPT_COMMAND ; echo "

#- -----------------------------------


# Change the window title of X terminals
case $TERM in
	screen*)
		# This is the escape sequence ESC k \w ESC \
			# Use path as title
		PATHTITLE='\[\ek\W\]\[\e\\\]'
		# Use program name as title
		PROGRAMTITLE='\[\ek\]\[\e\\\]'
		PS1="${PROGRAMTITLE}${PATHTITLE}${PS1}"
		;;
	xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|interix)
		# current dir
		PROMPT_COMMAND=''"$PROMPT_COMMAND"'; echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\007"'
		# program name
		trap 'echo -ne "\e]0;"; echo -n ${USER}@${HOSTNAME}: ${BASH_COMMAND}; echo -ne "\007"' DEBUG
		;;
	*)
		;;
esac

