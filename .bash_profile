source ~/.cmd_profile

source ~/dot-files/git-completion.sh
source ~/dot-files/ruby_identifier.sh

PS1='$(rvm_version)::\W$(__git_ps1 "(%s)")⚡ '
