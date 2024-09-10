

alias ll='ls -lh'
alias g='git'


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

export FZF_DEFAULT_OPTS="--exact"

# ctrl+r
function select-history() {
  BUFFER=$(history -n -r 1 | fzf --no-sort +m --query "$LBUFFER" --prompt="History > ")
  CURSOR=$#BUFFER
}
zle -N select-history
bindkey '^r' select-history

# git

# https://git-scm.com/book/ja/v2/%E4%BB%98%E9%8C%B2-A%3A-%E3%81%9D%E3%81%AE%E4%BB%96%E3%81%AE%E7%92%B0%E5%A2%83%E3%81%A7%E3%81%AEGit-Zsh%E3%81%A7Git%E3%82%92%E4%BD%BF%E3%81%86
# https://zsh.sourceforge.io/Doc/Release/User-Contributions.html#Version-Control-Information

autoload -Uz compinit && compinit


autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst

zstyle ':vcs_info:*' actionformats '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats  '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{5}]%f '
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}%r'
precmd () { vcs_info }
PS1='%F{5}[%F{2}%n%F{5}] %F{3}%3~ ${vcs_info_msg_0_}%f%# '


