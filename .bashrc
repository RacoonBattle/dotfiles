# .bashrc

# varialbes
export VISUAL='emacsclient -t -a ""'
export EDITOR="$VISUAL"
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

pathmunge /usr/local/opt/coreutils/libexec/gnubin before
pathmunge /sbin:/usr/sbin after
pathmunge $HOME/bin after
pathmunge $HOME/scripts after
pathmunge $HOME/Dropbox/scripts after
pathmunge $HOME/Dropbox/work/scripts after

export PATH
export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"

# completion
if [ -f /usr/share/bash-completion/bash_completion ]; then
	#fedora
	. /usr/share/bash-completion/bash_completion
fi
if [ -f /etc/profile.d/bash-completion.sh ]; then
	#gentoo
	. /etc/profile.d/bash-completion.sh
fi
if [ -f /usr/local/etc/bash_completion ]; then
	#macos brew
	. /usr/local/etc/bash_completion

fi

#my alias
alias ga='git add -A'
alias gb='git branch'
alias gc='git commit'
alias gd='git diff'
alias gl='git log'
alias go='git checkout'
alias gs='git status'
alias gu='git pull --rebase'
alias gw='git show'

alias ll='ls -lh --color=auto'
alias grep='grep --color'

alias e='emacsclient -t -a ""'
alias em='emacsclient -s gui -n'
alias vi=vim

alias ssh="TERM=xterm-256color ssh"
alias sshn='ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no'


# -------- colorful multi lines bash prompt ----------

# Reset
Color_Off="\[\033[0m\]"       # Text Reset

# Regular Colors
Black="\[\033[0;30m\]"        # Black
Red="\[\033[0;31m\]"          # Red
Green="\[\033[0;32m\]"        # Green
Yellow="\[\033[0;33m\]"       # Yellow
Blue="\[\033[0;34m\]"         # Blue
Purple="\[\033[0;35m\]"       # Purple
Cyan="\[\033[0;36m\]"         # Cyan
White="\[\033[0;37m\]"        # White

# Bold
BBlack="\[\033[1;30m\]"       # Black
BRed="\[\033[1;31m\]"         # Red
BGreen="\[\033[1;32m\]"       # Green
BYellow="\[\033[1;33m\]"      # Yellow
BBlue="\[\033[1;34m\]"        # Blue
BPurple="\[\033[1;35m\]"      # Purple
BCyan="\[\033[1;36m\]"        # Cyan
BWhite="\[\033[1;37m\]"       # White

# Underline
UBlack="\[\033[4;30m\]"       # Black
URed="\[\033[4;31m\]"         # Red
UGreen="\[\033[4;32m\]"       # Green
UYellow="\[\033[4;33m\]"      # Yellow
UBlue="\[\033[4;34m\]"        # Blue
UPurple="\[\033[4;35m\]"      # Purple
UCyan="\[\033[4;36m\]"        # Cyan
UWhite="\[\033[4;37m\]"       # White

# High Intensty
IBlack="\[\033[0;90m\]"       # Black
IRed="\[\033[0;91m\]"         # Red
IGreen="\[\033[0;92m\]"       # Green
IYellow="\[\033[0;93m\]"      # Yellow
IBlue="\[\033[0;94m\]"        # Blue
IPurple="\[\033[0;95m\]"      # Purple
ICyan="\[\033[0;96m\]"        # Cyan
IWhite="\[\033[0;97m\]"       # White

bash_git_prompt()
{
	git_branch=
	git_have_untracked_file=
	git_dir_dirty=
	git_prompt_symbol=
	git_color=

	# get current branch
	git_branch=$(git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')
	[ -n "$git_branch" ] || return

	# check if have untracked files
	git ls-files --exclude-standard --others 2>/dev/null \
		| grep -q ".*" && git_have_untracked_file=1

	# check if working directory clean (nothing to commit)
	git diff --quiet 2>/dev/null; test $? -eq 1 && git_dir_dirty=1

	if [ -n "$git_have_untracked_file" ] || [ -n "$git_dir_dirty" ]; then
		git_color=$Red
	else
		git_color=$Green
	fi

	if [ -n "$git_dir_dirty" ]; then
		git_prompt_symbol="*"
	fi

	echo "$git_color($git_branch$git_prompt_symbol)"
}

get_default_ip()
{
	default_ip4=`hostname`
	default_nic=$(ip route | awk '/default/{match($0,"dev ([^ ]+)",M); print M[1]; exit}')
	[ -n "$default_nic" ] && {
		default_ip4=$(ip addr show $default_nic \
		| awk '/inet .*'"$flg"'/{match($0,"inet ([0-9.]+)",M); print M[1]}')
	}
	echo $default_ip4
}

# multi line color
LINE1="[$BYellow\t $BGreen\u@\$(get_default_ip):$BBlue\w$Color_Off]"
LINE2="$Color_Off\n$ "
PROMPT_COMMAND=''"$PROMPT_COMMAND"'; export PS1="${LINE1} $(bash_git_prompt)${LINE2}"; echo'

# ------ Change the window title of X terminals ----------------
case $TERM in
	screen*)
		# This is the escape sequence ESC k \w ESC \
			# Use path as title
		PATHTITLE='\[\ek\W\]\[\e\\\]'
		# Use program name as title
		PROGRAMTITLE='\[\ek\]\[\e\\\]'
		PROMPT_COMMAND=''"$PROMPT_COMMAND"'; export PS1="${PROGRAMTITLE}${PATHTITLE}${PS1}"'
		;;
	xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|interix)
		# current dir
		PROMPT_COMMAND=''"$PROMPT_COMMAND"'; echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\007"'
		;;
	*)
		;;
esac
