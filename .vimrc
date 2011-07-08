" Created on Akash Manohar's machine
" This uses Vundle (https://github.com/gmarik/vundle) to manage vim scripts 

" no more compatible with vi (like I care ;)
set nocompatible

" stop beeping! STFU!
set vb

" write no backup files (don't know what these do, I still have .swp files)
set nowritebackup
set nobackup

" autoread from disk when file is changed outside of vim
set autoread

" set the working directory to the current file's path
if has('gui_running')
    set autochdir
endif


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

" Solarized colors
Bundle "altercation/vim-colors-solarized"

" end bundle list
filetype plugin indent on


"""""""""""""""""
" STATUS LINE 
"""""""""""""""""

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


" use macvim's three finger swipes to switch tabs
if has("gui_macvim")
    " mappings for normal mode
    nmap <SwipeLeft> :tabp<CR>
    nmap <SwipeRight> :tabn<CR>

    " mappings for insert mode
    imap <SwipeLeft> <ESC>:tabp<CR>
    imap <SwipeRight> <ESC>:tabn<CR>
endif


" w!! to sudo-save
cmap w!! %!sudo tee > /dev/null %


"""""""""""""""""""""""""""""""""""""
" AFTER PARTY
" I use this space to initiate stuff
""""""""""""""""""""""""""""""""""""""

syntax enable
if has("gui_running")
    " VIM IS RUNNING IN GUI

    " set font and font size in macvim
    set guifont=Monaco:h13
    
    set background=dark
    colorscheme solarized
else
    " VIM IS RUNNING IN COMMAND LINE
    
    " set bg and fg of line numbering
    " for cmd-line, 8 is dark grey, and 7 is light grey
    highlight LineNr ctermfg=8 ctermbg=7
    
    "set color scheme to desert
    colorscheme desert
endif


" TODO Later on steal some tricks from http://stackoverflow.com/questions/95072/what-are-your-favorite-vim-tricks/225852 as mentioned by gmarik
