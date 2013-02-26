# .bashrc

export PATH=$PATH:/sbin:~/bin

export PS1="\[\e[0m\]\[\e[33m\]\u\[\e[36;1m\] \W\[\e[0m\]\[\e[34;1m\] \$ \[\e[0m\]\[\e[37m\]"

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
#alias gd='git difftool'
alias gd='git diff'
alias gi='git add -i'
alias gl='git log --graph'
alias glp='git log --graph -p'
alias gp='git push'
alias gr='git ls-files -d |xargs git checkout --'
alias gs='git status'
alias gu='git pull'
alias gw='git show'

alias la='ls -lA --color=auto'
alias ll='ls -lh --color=auto'
alias lah='ls -lAh --color=auto'

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

