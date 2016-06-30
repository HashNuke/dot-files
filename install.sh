#/usr/bin/env bash

mkdir -p ~/projects
git clone git@github.com:HashNuke/dot-files.git ~/projects/dot-files

mkdir -p ~/bin
ln -s $HOME/projects/dot-files/start-tmux $HOME/bin/start-tmux

ln -s $HOME/projects/dot-files/.gdbinit $HOME/.gdbinit
ln -s $HOME/projects/dot-files/.vimrc $HOME/.vimrc
ln -s $HOME/projects/dot-files/.tmux.conf $HOME/.tmux.conf
ln -s $HOME/projects/dot-files/.gitconfig $HOME/.gitconfig
ln -s $HOME/projects/dot-files/.gitignore $HOME/.gitignore_global
ln -s $HOME/projects/dot-files/.emacs.d $HOME/.emacs.d
ln -s "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl" ~/bin/subl

echo "source $HOME/projects/dot-files/term-config.sh" >> ~/.bash_profile
echo "source $HOME/projects/dot-files/term-config.sh" >> ~/.bashrc

sudo apt-get install -y automake autoconf libreadline-dev libncurses-dev libssl-dev libyaml-dev libxslt-dev libffi-dev libtool unixodbc-dev
sudo apt-get install -y openjdk-8-jdk

wget -O- https://toolbelt.heroku.com/install-ubuntu.sh | sh

git clone git@github.com:HashNuke/asdf.git $HOME/.asdf
echo '. $HOME/.asdf/asdf.sh' >> ~/.bashrc
echo '. $HOME/.asdf/asdf.sh' >> ~/.bash_profile
source ~/.bash_profile
asdf plugin-add erlang git@github.com:HashNuke/asdf-erlang.git
asdf plugin-add elixir git@github.com:HashNuke/asdf-elixir.git
asdf plugin-add nodejs git@github.com:HashNuke/asdf-nodejs.git
asdf plugin-add ruby git@github.com:HashNuke/asdf-ruby.git


sudo printf "\n127.0.0.1 rails.dev\n127.0.0.1 ember.dev\n127.0.0.1 phoenix.dev\n127.0.0.1 jekyll.dev" >> /etc/hosts

git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

sed '$s|\}|'"    include $HOME/projects/dot-files/nginx-servers/*;"'}|g'
