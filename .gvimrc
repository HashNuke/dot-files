" Created on Akash Manohar's machine
" This uses Vundle (https://github.com/gmarik/vundle) to manage vim scripts 

" no more compatible with vi
set nocompatible

" no beep
set vb

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

filetype off

set rtp+=~/.vim/bundle/vundle/ 
call vundle#rc()

" Vundle can manage vundle"
Bundle "gmarik/vundle"

" I don't understand these, but it was there when I stole someone's vimrc
Bundle "L9"
Bundle "FuzzyFinder"
Bundle "ack.vim"

" NERDtree
Bundle "scrooloose/nerdtree"

" markdown syntax
Bundle "Markdown"

" ruby bob, ruby! that should come first
Bundle "vim-scripts/kkruby.vim"

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

" AutoCompleter
Bundle "SuperTab"

" Used to format text into tables. Useless for me, but still fashionable.
Bundle "godlygeek/tabular"

" end bundle list
filetype plugin indent on

"""""""""""""""""
" OTHER BELLS
"""""""""""""""""

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
set statusline+=%-14.(%l,%c%V%)%P\  " line, column number and percentage
set statusline+=%= " right align
set statusline+=%h%1*%m%r%w%0* " flags
set statusline+=[%{strlen(&ft)?&ft:'none'}, " filetype
set statusline+=%{&fileformat}]\  " file format

" god knows what this is, I'll ask him later
set ls=2


""""""""""""""""""""""""""""
" KEY AND COMMAND MAPPINGS
""""""""""""""""""""""""""""

"Set Mapleader. Like Emacs's mod key.
let mapleader = ","
let g:mapleader = ","

" <Leader> followed by the, key to open NERDTree
map <Leader>, :NERDTreeToggle<cr>

" use my Macbook's trackpad's gestures to switch tabs
nmap <SwipeLeft> :tabp<CR>
nmap <SwipeRight> :tabn<CR>

" maps w!! to sudo-save
cmap w!! %!sudo tee > /dev/null %
