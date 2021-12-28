if has("eval")
	let skip_defaults_vim = 1
endif

set nocompatible

" automatically indent new lines
set autoindent

" automatically write files when changing multiple files
set autowrite

" active line numbers
set number
set relativenumber

" turn col and row position in bottom right
set ruler

" show command and insert mode
set showmode

set tabstop=4 shiftwidth=4 softtabstop=4 noexpandtab
set smartindent 
set smarttab

if v:version >= 800
	" stop vim from silently messing with files that it shouldn't
	set nofixendofline

	" bettler ascii
	set listchars=space:*,trail:*,nbsp:*,extends:>,precedes:<,tab:\|>

	" don't do automatic folding
	set foldmethod=manual
	set nofoldenable
endif

" mark trailing spaces as erros
if has("match")
	match ErroMsg '\s\+$'
endif

set colorcolumn=80
set textwidth=72

" replace tabs with spaces automatically
set expandtab

" backuo stuff
"set backup
"set backupdir =$HOME/.vim/files/backup/
"set backupext =-vimbackup
"set backupskip =
"set directory =$HOME/.vim/files/swap/
"set updatecount =100
"set undofile
"set undodir =$HOME/.vim/files/undo/
"set viminfo ='100,n$HOME/.vim/files/info/viminfo
set nobackup
set noswapfile
set nowritebackup

" highlight search
set nohlsearch
"set hlsearch
set incsearch
set linebreak

set shortmess=aoOtTI

set viminfo='20,<1000,s1000

" no bracket matching or folding
if has("eval")
    let g:loaded_matchparen=1
endif
set noshowmatch

" wrap when searching
set wrapscan

set fo-=t   " don't auto-wrap text using text width
set fo+=c   " autowrap comments using textwidth with leader
set fo-=r   " don't auto-insert comment leader on enter in insert
set fo-=o   " don't auto-insert comment leader on o/O in normal
set fo+=q   " allow formatting of comments with gq
set fo-=w   " don't use trailing whitespace for paragraphs
set fo-=a   " disable auto-formatting of paragraph changes
set fo-=n   " don't recognized numbered lists
set fo+=j   " delete comment prefix when joining
set fo-=2   " don't use the indent of second paragraph line
set fo-=v   " don't use broken 'vi-compatible auto-wrapping'
set fo-=b   " don't use broken 'vi-compatible auto-wrapping'
set fo+=l   " long lines not broken in insert mode
set fo+=m   " multi-byte character line break support
set fo+=M   " don't add space before or after multi-byte char
set fo-=B   " don't add space between two multi-byte chars
set fo+=1   " don't break a line after a one-letter word

" stop complaints about switching buffer with changes
set hidden

set history=100

if has("syntax")
    syntax enable
endif

" fast scrolling
set ttyfast

" allow sensing the filetype
filetype plugin on

set background=dark

nnoremap confe :e $HOME/.vimrc<CR>
nnoremap confr :source $HOME/.vimrc<CR>
 
set ruf=%30(%=%#LineNr#%.50F\ [%{strlen(&ft)?&ft:'none'}]\ %l:%c\ %p%%%)

"colorscheme pablo

highlight Comment cterm=italic gui=italic
set clipboard=unnamedplus
set cursorline
set showcmd

let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
