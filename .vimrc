" Created on Akash Manohar's machine
" This uses Vundle (https://github.com/gmarik/vundle) to manage vim scripts 

" no more compatible with vi
set nocompatible

" write no backup files
set nowritebackup
set nobackup

" autoread from disk when file is changed outside of vim
set autoread

" i am on a mac. syntax highlighting did not work until i added this
syntax on
colorscheme desert

" set the working directory to the current file's path
set autochdir

"Set Mapleader
let mapleader = ","
let g:mapleader = ","

"""""""""""""""""""""""""""""
" TABing and INDENTing stuff
"""""""""""""""""""""""""""""

set tabstop=4
set shiftwidth=4

" tabs are actually multiple spaces
set expandtab

" auto and smart indentation
set autoindent
set smartindent

" C-style indentation
set cindent

" backspacing
set backspace=indent,eol,start

" turn line numbering on
set number

" set font and font size in macvim
set guifont=Monaco:h13

"""""""""""
" SEARCH
"""""""""""

" ignore case for searches
set ignorecase

" highlight search
set hlsearch

""""""""""""
" PASTING
""""""""""""

" set key to toggle paste
set pastetoggle=<F2>

""""""""""""
" FILETYPES
""""""""""""

au BufRead,BufNewFile {Gemfile,Rakefile,Capfile,*.rake,config.ru}  set ft=ruby
au BufRead,BufNewFile {*.md,*.mkd,*.markdown}  set ft=markdown
au Bufread,BufNewFile {*.as}  set filetype=actionscript
au BufRead,BufNewFile {*.html.haml,*.haml}  set ft=haml
au BufRead,BufNewFile {*.rkt,*.scm,*.lp}  set ft=scheme
au BufRead,BufNewFile {*.tt}  set ft=treetop


""""""""""
" BUNDLES
""""""""""

filetype on

set rtp+=~/.vim/bundle/vundle/ 
call vundle#rc()

" Vundle can manage vundle"
Bundle "gmarik/vundle"

Bundle "L9"
Bundle "FuzzyFinder"
Bundle "ack.vim"

" File opener
Bundle "Quicksilver.vim"

" NERDtree
Bundle "scrooloose/nerdtree"

" markdown syntax
Bundle "Markdown"

" ruby bob, ruby! that should come first
Bundle "vim-scripts/kkruby.vim"

" Command-T file finder
" disabled coz it needs the same ruby as MacVim build
" Bundle "wincent/Command-T"

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

" scss
Bundle "cakebaker/scss-syntax.vim"

" coffeescript
Bundle "kchmck/vim-coffee-script"

" ActionScript
Bundle "actionscript.vim--Cuss"

" Treetop
Bundle "nanki/treetop.vim"

" Utility
Bundle "SuperTab"
Bundle "godlygeek/tabular"

" end bundle list
filetype plugin indent on

"""""""""""""""""
" OTHER BELLS
"""""""""""""""""

" NOTE: if some plugins fail to work, put the config *between* lines:
" filetype off
" "Bundles here
" filetype plugin indent on

" TODO Steal some tricks from http://stackoverflow.com/questions/95072/what-are-your-favorite-vim-tricks/225852 as mentioned by gmarik

" set bg and fg of line numbering
" for cmd-line, 8 is dark grey, and 7 is light grey
highlight LineNr ctermfg=8 ctermbg=7

" for GUI, use hex
highlight LineNr guifg=#CCCCCC guibg=#666666

" make filename and status always visible
set modeline

"Status line visual fluff
set laststatus=2
set statusline=
set statusline+=%f " file name
set statusline+=\ \  " some whitespace 
set statusline+=%-14.(%l,%c%V%)%P\  " offset
set statusline+=%= " right align
set statusline+=%h%1*%m%r%w%0* " flags
set statusline+=[%{strlen(&ft)?&ft:'none'}, " filetype
set statusline+=%{&fileformat}]\  " file format

set ls=2

""""""""""""""""""""""""""""
" KEY AND COMMAND MAPPINGS
""""""""""""""""""""""""""""

map <Leader>, :NERDTreeToggle<cr>

" maps w!! to sudo save
cmap w!! %!sudo tee > /dev/null %
