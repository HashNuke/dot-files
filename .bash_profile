source ~/.cmd_profile

source ~/dot-files/git-completion.sh
source ~/dot-files/ruby_identifier.sh
source /Users/akashmanohar/.pythonbrew/etc/bashrc

export PATH=/usr/local/bin:/usr/local/sbin:$PATH

PS1='$(rvm_version)::\W$(__git_ps1 "(%s)")⚡ '
