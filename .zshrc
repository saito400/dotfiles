# load zgen
source "${HOME}/.zgen/zgen.zsh"

# check if there's no init script
if ! zgen saved; then
    echo "Creating a zgen save"

    zgen oh-my-zsh

    # plugins
    zgen oh-my-zsh plugins/git
    zgen load zsh-users/zsh-syntax-highlighting
    zgen load zsh-users/zsh-completions

    # theme
    zgen oh-my-zsh themes/clean

    # save all to init script
    zgen save
fi

function peco-select-history() {
    local tac
    if which tac > /dev/null; then
        tac="tac"
    else
        tac="tail -r"
    fi
    BUFFER=$(\history -n 1 | eval $tac | awk '!a[$0]++' | peco --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle clear-screen
}
zle -N peco-select-history

bindkey '^r' peco-select-history

# emacs mode
bindkey -e

eval "$(rbenv init -)"

alias less='less -N'
alias emacs='emacs -nw'
