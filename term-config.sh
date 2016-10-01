# export TERM=xterm-256color
unamestr=`uname`

export PATH=$HOME/bin:$PATH
if [[ "$unamestr" == 'Linux' ]]; then
  export LC_ALL="en_US.UTF-8"
fi

# Emacs
# alias emacs="emacs -nw"
alias ec="emacsclient -nw"
alias em="emacs -nw"

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

PROMPT='
%{$fg[magenta]%}%n%{$reset_color%} at %{$fg[yellow]%}%m%{$reset_color%} in %{$fg_bold[green]%}$(collapse_pwd) %{$reset_color%}$(hg_prompt_info)$(git_prompt_info)
$(virtualenv_info)$(prompt_char) '