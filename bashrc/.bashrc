# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# environment variables
export VISUAL=vi
export EDITOR="$VISUAL"
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
export WORKSPACES="$HOME/ExtraDrive/containers/Workspaces"
export ZETDIR="$HOME/ExtraDrive/Repos/github.com/zet"

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

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'


# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

unalias -a
alias '?'=duck
alias d='date'
alias la='ls -lhaF'
alias l='ls -CF'
alias x='exit'
alias vi='vim'
alias c='printf "\e[H\e[2J"'
alias c='printf "\e[H\e[2J"'
alias df='df -h'
alias free='free -h'
alias scripts='cd $SCRIPTS'
alias zet='cd $ZETDIR'
alias repos='cd $REPOS' 
alias dot='cd $DOTFILES'
alias pomo='~/go/bin/pomo'
alias documents='cd $DOCUMENTS'
alias downloads='cd $DOWNLOADS'
alias desktop='cd $DESKTOP'
alias chmodx='chmod +x'
alias top=htop
alias ..='cd ..'
alias ....='cd ../..'
alias ls='ls -h --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias more=less
alias k=kubectl
alias mk=minikube

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

# PATH 

pathappend() {
  declare arg
  for arg in "$@"; do
    test -d "$arg" || continue
    PATH=${PATH//":$arg:"/:}
    PATH=${PATH/#"$arg:"/}
    PATH=${PATH/%":$arg"/}
    export PATH="${PATH:+"$PATH:"}$arg"
  done
} && export pathappend

pathprepend() {
  for arg in "$@"; do
    test -d "$arg" || continue
    PATH=${PATH//:"$arg:"/:}
    PATH=${PATH/#"$arg:"/}
    PATH=${PATH/%":$arg"/}
    export PATH="$arg${PATH:+":${PATH}"}"
  done
} && export pathprepend

# remember last arg will be first in path
pathprepend \
  "$HOME/Appimages/" \
  /usr/local/go/bin \
  "$HOME/.local/bin" \
  "$SCRIPTS" \
  "$PYENV_ROOT/bin"

pathappend \
  /usr/local/opt/coreutils/libexec/gnubin \
  /mingw64/bin \
  /usr/local/bin \
  /usr/local/sbin \
  /usr/local/games \
  /usr/games \
  /usr/sbin \
  /usr/bin \
  /snap/bin \
  /sbin \
  /bin

# Smart cd
export CDPATH=".:$DOTFILES:$EXTRADRIVE:$REPOS:$REPOS/github.com:$HOME"

#Prompt

# Show git branch in terminal
#parse_git_branch() {
#     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
#}

parse_git_branch() {
	local s='';
	local branchName='';

	# Check if the current directory is in a Git repository.
	if [ $(git rev-parse --is-inside-work-tree &>/dev/null; echo "${?}") == '0' ]; then

		# check if the current directory is in .git before running git checks
		if [ "$(git rev-parse --is-inside-git-dir 2> /dev/null)" == 'false' ]; then

			# Ensure the index is up to date.
			git update-index --really-refresh -q &>/dev/null;

			# Check for uncommitted changes in the index.
			if ! $(git diff --quiet --ignore-submodules --cached); then
				s+='+';
			fi;

			# Check for unstaged changes.
			if ! $(git diff-files --quiet --ignore-submodules --); then
				s+='!';
			fi;

			# Check for untracked files.
			if [ -n "$(git ls-files --others --exclude-standard)" ]; then
				s+='?';
			fi;

			# Check for stashed files.
			if $(git rev-parse --verify refs/stash &>/dev/null); then
				s+='$';
			fi;

		fi;

		# Get the short symbolic ref.
		# If HEAD isn’t a symbolic ref, get the short SHA for the latest commit
		# Otherwise, just give up.
		branchName="$(git symbolic-ref --quiet --short HEAD 2> /dev/null || \
			git rev-parse --short HEAD 2> /dev/null || \
			echo '(unknown)')";

		[ -n "${s}" ] && s=" [${s}]";

		echo -e "${1}${branchName}${2}${s}";
	else
		return;
	fi;
}

#prompt color setup

red=$(tput setaf 196);
orange=$(tput setaf 166);
yellow=$(tput setaf 228);
green=$(tput setaf 71);
white=$(tput setaf 15);
blue=$(tput setaf 153);
grey=$(tput setaf 238);
pink=$(tput setaf 205);
bold=$(tput bold);
reset=$(tput sgr0);

#PS1="\[${bold}\]\n";
#PS1+="\[${orange}\]\u"; #user
#PS1+="\[${white}\] at ";
#PS1+="\[${yellow}\]\h" #host
#PS1+="\[${white}\] in ";
#PS1+="\[${green}\]\W"; #dir
#PS1+="\$(parse_git_branch \"\[${white}\] on \[${red}\]\" \"\[${blue}\]\")"; # Git repository details
#PS1+="\n";
#PS1+="\[${white}\]\$ \[${reset}\]";
#export PS1;

PS1="\n"; #add space for virtual environment stuff
PS1+="\[${grey}\]╔ ";
PS1+="\[${pink}\]\u"; #user
PS1+="\[${grey}\]:";
PS1+="\[${blue}\]\W"; #dir
PS1+="\$(parse_git_branch \"\[${white}\] on \[${red}\]\" \"\[${blue}\]\")"; # Git repository details
PS1+="\n";
PS1+="\[${grey}\]╚ ";
PS1+="\[${white}\]\$ \[${reset}\]";
export PS1;

eval "$(pyenv init --path)"
