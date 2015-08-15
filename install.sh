#/usr/bin/env bash

git clone git@github.com:HashNuke/dot-files.git ~/projects/dot-files

ln -s $HOME/projects/dot-files/.vimrc $HOME/.vimrc
ln -s $HOME/projects/dot-files/.tmux.conf $HOME/.tmux.conf
ln -s $HOME/projects/dot-files/.gitconfig $HOME/.gitconfig
ln -s $HOME/projects/dot-files/.gitignore $HOME/.gitignore_global
ln -s $HOME/projects/dot-files/.emacs.d $HOME/.emacs.d
printf "\nsource $HOME/projects/dot-files/term-config.sh" >> ~/.bash_profile

sudo printf "\n127.0.0.1 rails.dev\n127.0.0.1 ember.dev\n127.0.0.1 phoenix.dev\n127.0.0.1 jekyll.dev" >> /etc/hosts

git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

sed '$s|\}|'"    include $HOME/projects/dot-files/nginx-servers/*;"'}|g'
