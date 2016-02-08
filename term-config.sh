source $HOME/projects/dot-files/git-prompt.sh

# export TERM=xterm-256color

export PATH=$HOME/bin:$PATH

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]] || [[ "$unamestr" == 'Darwin' ]]; then
  export PS1=$'\n\[\e[0;32m\]\w\[\e[1;31m\]$(__git_ps1 ":%s") \n\[\e[0;36m\]\xe2\x98\x85 \[\e[0m\]'
# elif [[ "$unamestr" == 'FreeBSD' ]]; then
fi

# Emacs
# alias emacs="emacs -nw"
alias ec="emacsclient -nw"

alias jr="jruby -S"
alias jrbe="jruby -S bundle exec"

# Ruby
alias be="bundle exec"

alias gl="git lg"
alias gs="git status -sb"
alias gp="git push"
alias gu="git pull"

alias n=nano
alias ia="open $1 -a /Applications/iA\ Writer\ Pro.app"
