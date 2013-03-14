# .bashrc
export PATH=$PATH:/sbin:~/bin
export VISUAL=vim
export EDITOR=vim

# completion
if [ -f /etc/bash_completion ]
then	#fedora
	. /etc/bash_completion
fi
if [ -f /etc/profile.d/bash-completion.sh ]
then	#gentoo
	. /etc/profile.d/bash-completion.sh
fi

# history append
shopt -s histappend 
PROMPT_COMMAND='history -a' 

#my alias
alias ga='git add -A'
alias gc='git commit -a'
alias gcm='git commit -m'
alias gd='git diff'
alias gi='git add -i'
alias gl='git log --graph'
alias glp='git log --graph -p'
alias gp='git push'
alias gr='git ls-files -d |xargs git checkout --'
alias gs='git status'
alias gu='git pull'
alias gw='git show'

alias la='ls -lAh --color=auto'
alias ll='ls -lh --color=auto'
alias vi='vim'

#set the screen title  
case $TERM in  
screen*)  
    # This is the escape sequence ESC k \w ESC \  
    # Use path as title  
    PATHTITLE='\[\ek\W\]\[\e\\\]'  
    # Use program name as title  
    PROGRAMTITLE='\[\ek\]\[\e\\\]'  
    PS1="${PROGRAMTITLE}${PATHTITLE}${PS1}"  
    ;;  
*)  
    ;;  
esac  

# ===== Color Bash Prompt ===
# Reset
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

# Underline
UBlack='\e[4;30m'       # Black
URed='\e[4;31m'         # Red
UGreen='\e[4;32m'       # Green
UYellow='\e[4;33m'      # Yellow
UBlue='\e[4;34m'        # Blue
UPurple='\e[4;35m'      # Purple
UCyan='\e[4;36m'        # Cyan
UWhite='\e[4;37m'       # White

# Background
On_Black='\e[40m'       # Black
On_Red='\e[41m'         # Red
On_Green='\e[42m'       # Green
On_Yellow='\e[43m'      # Yellow
On_Blue='\e[44m'        # Blue
On_Purple='\e[45m'      # Purple
On_Cyan='\e[46m'        # Cyan
On_White='\e[47m'       # White

# High Intensity
IBlack='\e[0;90m'       # Black
IRed='\e[0;91m'         # Red
IGreen='\e[0;92m'       # Green
IYellow='\e[0;93m'      # Yellow
IBlue='\e[0;94m'        # Blue
IPurple='\e[0;95m'      # Purple
ICyan='\e[0;96m'        # Cyan
IWhite='\e[0;97m'       # White

# Bold High Intensity
BIBlack='\e[1;90m'      # Black
BIRed='\e[1;91m'        # Red
BIGreen='\e[1;92m'      # Green
BIYellow='\e[1;93m'     # Yellow
BIBlue='\e[1;94m'       # Blue
BIPurple='\e[1;95m'     # Purple
BICyan='\e[1;96m'       # Cyan
BIWhite='\e[1;97m'      # White

# High Intensity backgrounds
On_IBlack='\e[0;100m'   # Black
On_IRed='\e[0;101m'     # Red
On_IGreen='\e[0;102m'   # Green
On_IYellow='\e[0;103m'  # Yellow
On_IBlue='\e[0;104m'    # Blue
On_IPurple='\e[0;105m'  # Purple
On_ICyan='\e[0;106m'    # Cyan
On_IWhite='\e[0;107m'   # White

function parse_git_branch {
   git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
} 
sq_color="$IBlack"
DIR_INFO="$ICyan\$(/bin/ls -1 | /usr/bin/wc -l | /bin/sed 's: ::g') files$IYellow \$(/bin/ls -lah | /bin/grep -m 1 total | /bin/sed 's/total //')"
LINE1="$sq_color[$DIR_INFO$sq_color]-$Red\$(parse_git_branch)$sq_color[$IGreen\w$sq_color]"
LINE2="$IWhite\u$White@$IWhite\h$BBlue $ $Color_Off"
PS1="$sq_color\342\224\214\342\224\200$LINE1$sq_color\n\342\224\224\342\224\200\342\224\200>$LINE2"
# ========
