
set -x LSCOLORS gxfxcxdxbxegedabagacad

set -x GOPATH $HOME/.go
set -x PATH $GOPATH/bin $PATH

rbenv init - fish | source

set -x EDITOR emacs

set -x FZF_DEFAULT_OPTS '-e'

alias g git
alias less 'less -N'
