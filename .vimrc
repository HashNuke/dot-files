" no more compatible with vi
set nocompatible

" write no backup files
set nowritebackup
set nobackup

" autoread from disk when file is changed outside of vim
set autoread

"""""""""""""""""""""""""""""
" TABing and INDENTing stuff
"""""""""""""""""""""""""""""

set tabstop=2
set shiftwidth=2

" tabs are actually multiple spaces
set expandtab

" auto and smart indentation
set autoindent
set smartindent

" C-style indentation
set cindent

" backspacing
set backspace=indent,eol,start


"""""""""""
" SEARCH
"""""""""""

" ignore case for searches
set ignorecase

" highlight search
set hlsearch



""""""""""""
" FILETYPES
""""""""""""

au BufRead,BufNewFile {Gemfile,Rakefile,Capfile,*.rake,config.ru}  set ft=ruby
au BufRead,BufNewFile {*.md,*.mkd,*.markdown}                      set ft=markdown

""""""""""
" BUNDLES
""""""""""

filetype on

set rtp+=~/.vim/vundle.git/ 
call vundle#rc()

Bundle "L9"
Bundle "FuzzyFinder"
Bundle "ack.vim"

" markdown syntax
Bundle "Markdown"

Bundle "git://git.wincent.com/command-t.git"

" cucumber
Bundle "cucumber.zip"

" rails
Bundle "rails.vim"

" jquery
Bundle "jQuery"

" git
Bundle "git.zip"
Bundle "fugitive.vim"

" haml
Bundle "tpope/vim-haml"

" coffeescript
Bundle "kchmck/vim-coffee-script"

" Utility
Bundle "SuperTab"

" end bundle list
filetype plugin indent on

" NOTE: if some plugins fail to work, put the config *between* lines:
" filetype off
" "Bundles here
" filetype plugin indent on

set background=dark

" TODO Steal some tricks from http://stackoverflow.com/questions/95072/what-are-your-favorite-vim-tricks/225852 as mentioned by gmarik

