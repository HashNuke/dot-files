#/usr/bin/env bash

mkdir -p ~/bin

git clone git@github.com:HashNuke/dot-files.git

ln -s $HOME/projects/dot-files/.vimrc $HOME/.vimrc
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

ln -s $HOME/projects/dot-files/.psqlrc $HOME/.psqlrc
ln -s $HOME/projects/dot-files/.tmux.conf $HOME/.tmux.conf
ln -s $HOME/projects/dot-files/bin/start-tmux $HOME/bin/start-tmux
ln -s $HOME/projects/dot-files/bin/psqlc $HOME/bin/psqlc

# Install oh-my-zsh
# sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# Skip if linux; Use default theme
ln -s $HOME/projects/dot-files/honukai.zsh-theme $HOME/.oh-my-zsh/themes/honukai.zsh-theme

ln -s $HOME/projects/dot-files/.gdbinit $HOME/.gdbinit
ln -s $HOME/projects/dot-files/.gitconfig $HOME/.gitconfig
ln -s $HOME/projects/dot-files/.gitignore $HOME/.gitignore_global
ln -s $HOME/projects/dot-files/.emacs.d $HOME/.emacs.d

echo "source $HOME/projects/dot-files/term-config.sh" >> ~/.zshrc

## Ubuntu
# sudo apt-get install -y zsh git-core make automake autoconf build-essential libreadline-dev libncurses-dev libssl-dev libyaml-dev libxslt-dev libffi-dev libtool unixodbc-dev unzip wget tshark
# sudo apt-get install -y openjdk-8-jdk
# chsh -s `which zsh`

# wget -O- https://toolbelt.heroku.com/install-ubuntu.sh | sh

git clone git@github.com:HashNuke/asdf.git $HOME/.asdf
echo '. $HOME/.asdf/asdf.sh' >> ~/.zshrc
source ~/.zshrc
asdf plugin-add erlang git@github.com:HashNuke/asdf-erlang.git
asdf plugin-add elixir git@github.com:HashNuke/asdf-elixir.git
asdf plugin-add nodejs git@github.com:HashNuke/asdf-nodejs.git
asdf plugin-add ruby git@github.com:HashNuke/asdf-ruby.git
