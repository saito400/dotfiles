
# ls
alias ll='ls -lh'

# history
HISTFILE=~/.zsh_history
HISTSIZE=10000
AVEHIST=10000
setopt share_history
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt append_history
setopt inc_append_history
setopt hist_no_store
setopt hist_reduce_blanks
ZDOTDIR=$HOME/.config/zsh

fzf-ghq() {
  local repo=$(ghq list | fzf --preview "ghq list --full-path --exact {} | xargs exa -h --long --icons --classify --git --no-permissions --no-user --no-filesize --git-ignore --sort modified --reverse --tree --level 2")
  if [ -n "$repo" ]; then
    repo=$(ghq list --full-path --exact $repo)
    BUFFER="cd ${repo}"
    zle accept-line
  fi
  zle clear-screen
}
zle -N fzf-ghq

bindkey '^g' fzf-ghq

# ctrl+r
function select-history() {
  BUFFER=$(history -n -r 1 | fzf --no-sort +m --query "$LBUFFER" --prompt="History > ")
  CURSOR=$#BUFFER
}
zle -N select-history
bindkey '^r' select-history
