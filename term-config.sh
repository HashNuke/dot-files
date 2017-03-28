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

function iawriter {
  if [ ! -f $1 ]; then
    touch $1
  fi
  open -a iA\ Writer $1
}


# Steve Losh's ZSH colors

function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}

function prompt_char {
    git branch >/dev/null 2>/dev/null && echo '±' && return
    echo '○'
}


function virtualenv_info {
    [ $VIRTUAL_ENV ] && echo '('`basename $VIRTUAL_ENV`') '
}


ZSH_THEME_GIT_PROMPT_PREFIX=" on %{$fg[magenta]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[green]%}!"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[green]%}?"
ZSH_THEME_GIT_PROMPT_CLEAN=""

PROMPT='
%{$fg[magenta]%}%n%{$reset_color%} at %{$fg[yellow]%}%m%{$reset_color%} in %{$fg_bold[green]%}$(collapse_pwd)%{$reset_color%}$(git_prompt_info)
$(virtualenv_info)$(prompt_char) '

GPG_TTY=$(tty)
export GPG_TTY

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export PATH=$PATH:$HOME/.local/bin
