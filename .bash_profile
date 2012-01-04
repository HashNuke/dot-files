source ~/.cmd_profile

source ~/dot-files/color_names.sh
source ~/dot-files/git-completion.sh
source ~/dot-files/ruby_identifier.sh
source /Users/akashmanohar/.pythonbrew/etc/bashrc

export PATH=/usr/local/bin:/usr/local/sbin:$PATH

# Displays 1.9.3@gemset::dir(branch)
# PS1='$(rvm_version)::\W$(__git_ps1 "(%s)")⚡ '
# PS1='$(rvm_version)$(__git_ps1 "(%s)")⚡ '

# Displays [gemset dir:branch]
PS1="\\[$Cyan\][\[$Green\]$(~/.rvm/bin/rvm-prompt g | cut -c2-15) \[$IYellow\]\W\[$IPurple\]:$(__git_ps1 "%s")\[$Cyan\]] ⚡ \[$White\]"

