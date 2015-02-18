# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/Users/saito/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

alias less='less -N'
alias ll='ls -al'
eval "$(rbenv init -)"
setopt share_history

export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
