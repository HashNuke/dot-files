" Created on Akash Manohar's machine
" This uses Vundle (https://github.com/gmarik/vundle) to manage vim scripts 

" Gets powerline working under tmux
set t_Co=256

" no more compatible with vi (like I care ;)
set nocompatible

" STFU! stop beeping!
set vb

" enable trackpad/mouse scrolling
set mouse=a

" write no backup files (don't know what these do, I still have .swp files)
set nowritebackup
set nobackup

" autoread from disk when file is changed outside of vim
set autoread

" incrementally search while typing
set incsearch

" highlight current line
set nocursorline

" set the working directory to the current file's path
" if has('gui_running')
"    set autochdir
" endif


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

"""""""""
" Ignores
"""""""""

set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*/node_modules/*


""""""""""""
" FILETYPES
""""""""""""

au BufRead,BufNewFile {Gemfile,Rakefile,Capfile,Vagrantfile,*.rb,*.rake,config.ru}  set ft=ruby
au BufRead,BufNewFile {*.md,*.mkd,*.markdown}  set ft=markdown
au Bufread,BufNewFile {*.as}  set filetype=actionscript
au BufRead,BufNewFile {*.html.haml,*.haml}  set ft=haml
au BufRead,BufNewFile {*.rkt,*.scm}  set ft=scheme
au BufRead,BufNewFile {*.tt}  set ft=treetop
au BufNewFile,BufRead *.less set filetype=less

""""""""""
" BUNDLES
""""""""""

filetype off

set rtp+=~/.vim/bundle/vundle/ 
call vundle#rc()

" Vundle can manage vundle"
Bundle "gmarik/vundle"

" Provides some functions to write in vimscript
Bundle "L9"

" markdown syntax
Bundle "Markdown"

" ruby bob, ruby! that should come first
Bundle "vim-scripts/kkruby.vim"

" rails
Bundle "tpope/vim-rails"

" haml
Bundle "tpope/vim-haml"

" scss
Bundle "cakebaker/scss-syntax.vim"

" coffeescript
Bundle "kchmck/vim-coffee-script"

" ZenCoding
Bundle "mattn/zencoding-vim"

" Command-P
Bundle "kien/ctrlp.vim"

" NERDCommenter to comment out code
Bundle "scrooloose/nerdcommenter"

" Gist
Bundle "mattn/gist-vim"

" Solarized colors
Bundle "altercation/vim-colors-solarized"

" git
Bundle "tpope/vim-fugitive"

" Vim powerline
Bundle "Lokaltog/vim-powerline"

" Erlang
Bundle 'jimenezrick/vimerl'

" end bundle list
filetype plugin indent on

"""""""""""""""""
" STATUS LINE 
"""""""""""""""""

" make filename and status always visible
set modeline

"Status line visual fluff
"set laststatus=2
"set statusline=
"set statusline+=%f   " file name
"set statusline+=\ \  " some whitespace 
"set statusline+=%-14.(%l,%c%V%)%P\  " line, column number and percentage
"set statusline+=%=   " right align
"set statusline+=%h%1*%m%r%w%0* " flags
"set statusline+=[%{strlen(&ft)?&ft:'none'}, " filetype
"set statusline+=%{&fileformat}]\  " file format

" god knows what this is, I'll ask him later
set ls=2


"""""""""""""""""""""""
" FUNCTIONS
"""""""""""""""""""""""

" SetGitWD() will set the current working dir
" to the root of the git repo that the current file belongs to
" And this is my first function in vimscript
" TODO Borrow tricks from this function
" https://github.com/vim-scripts/ctrlp.vim/blob/master/autoload/ctrlp.vim#L641
function! SetGitWD()
    try
        let git_root = system("git rev-parse --show-toplevel")
        execute "cd! " git_root
        " echo "working directory is now" git_root
    catch
        "Do nothing for now
    endtry
endfunction

""""""""""""""""""""""""""""
" KEY AND COMMAND MAPPINGS
""""""""""""""""""""""""""""

let g:ctrlp_prompt_mappings = {
      \ 'AcceptSelection("e")': [],
      \ 'AcceptSelection("t")': ['<cr>', '<c-m>'],
      \ }

" Set Mapleader. Like Emacs's mod key.
let mapleader = ","
let g:mapleader = ","

" Let CtrlP manage the working directory
let g:ctrlp_working_path_mode = 2

" <Leader> followed by the / key to open NERDTree
map <Leader>/ :Vex<cr>

" use macvim's three finger swipes to switch tabs
if has("gui_macvim")
    " this is rendered useless by Lion's default gestures

    " mappings for normal mode
    nmap <SwipeLeft> :tabp<CR>
    nmap <SwipeRight> :tabn<CR>
    nmap <SwipeUp> :bp<CR>
    nmap <SwipeDown> :bn<CR>

    " mappings for insert mode
    imap <SwipeLeft> <ESC>:tabp<CR>
    imap <SwipeRight> <ESC>:tabn<CR>
    imap <SwipeUp> <ESC>:bp<CR>
    imap <SwipeDown> <ESC>:bn<CR>
endif

" w!! to sudo-save
cmap w!! %!sudo tee > /dev/null %


""""""""""""""""""""""""""""""""""""""
" AFTER PARTY
" I use this space to initiate stuff
""""""""""""""""""""""""""""""""""""""

" enable syntax highlighting
syntax enable

" check if Vim is running in CLI or in GUI mode for specific settings
if has("gui_running")
    " VIM IS RUNNING IN GUI
    " set font and font size in macvim
    set guifont=Monaco:h13
    set background=dark
    colorscheme railscasts
else
    " i keep changing this, so let's keep this seperate
    set background=dark
    "let g:solarized_termcolors = 256 
    let g:solarized_visibility = "high" 
    let g:solarized_contrast = "high" 
    colorscheme solarized
endif

" set the working dir to the root of the git repo
" au BufEnter * call SetGitWD()

" TODO Later on steal some tricks from http://stackoverflow.com/questions/95072/what-are-your-favorite-vim-tricks/225852 as mentioned by gmarik
" and steal more from https://bitbucket.org/sjl/dotfiles/src/tip/vim/.vimrc
