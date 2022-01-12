# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# environment variables
export EDITOR=vi
export VISUAL=vi
export EDITOR_PREFIX=vi
export DOTFILES="$HOME/dot"
export SCRIPTS="$DOTFILES/scripts"
export REPOS="$HOME/ExtraDrive/Repos"
export PYENV_ROOT="$HOME/.pyenv"
export DOCUMENTS="$HOME/Documents"
export DOWNLOADS="$HOME/Downloads"
export DESKTOP="$HOME/Desktop"
export EXTRADRIVE="$HOME/ExtraDrive"
export POETRY="$HOME/.local/bin"

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
export HISTCONTROL=ignoreboth
# append to the history file, don't overwrite it
shopt -s histappend
# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=5000
export HISTFILESIZE=10000

# vi mode on bash
set -o vi

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize
# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar
# other bash shell options
shopt -s expand_aliases
shopt -s globstar
shopt -s dotglob
shopt -s extglob

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi


# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'


# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

#unalias -a
alias '?'=duck
alias d='date'
alias la='ls -lhaF'
alias l='ls -CF'
alias x='exit'
alias vi='vim'
alias c='printf "\e[H\e[2J"'
alias df='df -h'
alias free='free -h'
alias scripts='cd $SCRIPTS'
alias repos='cd $REPOS' 
alias dot='cd $DOTFILES'
alias pomo='~/go/bin/pomo'
alias documents='cd $DOCUMENTS'
alias downloads='cd $DOWNLOADS'
alias desktop='cd $DESKTOP'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# Changes to PATH go here
export PATH="$PYENV_ROOT/bin:$POETRY:$SCRIPTS:$PATH:/usr/local/go/bin"

# Smart cd
export CDPATH=".:$DOTFILES:$EXTRADRIVE:$REPOS:$HOME"

# Show git branch in terminal
parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

export PS1="\[\033[00;38;5;240m\]╔ \[\033[01;38;5;219m\]\u\[\033[37m\]:\[\033[38;5;75m\]\W\[\033[01;31m\]\$(parse_git_branch)
\[\033[00;38;5;240m\]╚\[\033[38;5;75m\] $\[\033[0m\] "

eval "$(pyenv init --path)"
