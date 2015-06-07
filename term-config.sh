source $HOME/projects/dot-files/git-prompt.sh

export PATH=$HOME/bin:$PATH

export PS1=$'\n\[\e[0;32m\]\w\[\e[1;31m\]$(__git_ps1 ":%s") \n\[\e[0;36m\]\xe2\x98\x85 \[\e[0m\]'

# Emacs
# alias emacs="emacs -nw"
alias ec="emacsclient -t"

alias jr="jruby -S"
alias jrbe="jruby -S bundle exec"

# Ruby
alias be="bundle exec"

alias gl="git lg"
alias gs="git status -sb"
alias gp="git push"
alias gu="git pull"
