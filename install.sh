#/usr/bin/env bash

git clone git@github.com:HashNuke/dot-files.git ~/projects/dot-files

ln -s $HOME/projects/dot-files/.vimrc $HOME/.vimrc
ln -s $HOME/projects/dot-files/.tmux.conf $HOME/.tmux.conf
ln -s $HOME/projects/dot-files/.gitconfig $HOME/.gitconfig
ln -s $HOME/projects/dot-files/.gitignore $HOME/.gitignore_global
echo "source $HOME/projects/dot-files/term-config.sh" >> ~/.bash_profile

git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
