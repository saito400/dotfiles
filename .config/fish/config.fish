
set -x GOPATH $HOME/.go
set -x PATH $GOPATH/bin $PATH

rbenv init - fish | source

set -x EDITOR emacs

alias g git

