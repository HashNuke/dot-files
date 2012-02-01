source ~/.cmd_profile

source ~/dot-files/color_names.sh
source ~/dot-files/git-completion.sh
source ~/dot-files/ruby_identifier.sh
source /Users/akashmanohar/.pythonbrew/etc/bashrc

export PATH=/usr/local/bin:/usr/local/sbin:$PATH

# Displays 1.9.3@gemset::dir(branch)
PS1='\[\e[0;36m\][\[\e[0;30m\]$(rvm_version) \[\e[1;33m\]\W\[\e[0;35m\]$(__git_ps1 ":%s")\[\e[0;36m\]]⚡ \[\e[0;37m\]'
