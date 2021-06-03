let mapleader = ","

if ! filereadable(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim"'))
	echo "Downloading junegunn/vim-plug to manage plugins..."
	silent !mkdir -p ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/
	silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim
	autocmd VimEnter * PlugInstall
endif

" Plugins go here:
call plug#begin(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/plugged"'))
"	Plug 'vim-scripts/paredit.vim'
	Plug 'eraserhd/parinfer-rust', {'do': 'cargo build --release', 'for': 'clojure'}
    Plug 'guns/vim-sexp', {'for': 'clojure'}
    Plug 'tpope/vim-sexp-mappings-for-regular-people', {'for': 'clojure'}
    Plug 'tpope/vim-surround'
	Plug 'tpope/vim-repeat'
	Plug 'tpope/vim-speeddating'
	Plug 'tpope/vim-commentary'
call plug#end()

if exists('g:vscode')
    noremap <Leader>c :call VSCodeNotify('clover.connectSocketRepl')<CR>  
endif

" Some basics:
	set clipboard+=unnamedplus
	set number relativenumber

	set nocompatible
	filetype plugin on
	syntax on
	set encoding=utf-8
	
" Perform dot commands over visual blocks:
	vnoremap . :normal .<CR>
" Splits open at the bottom and right
	set splitbelow splitright

" Replace all is aliased to S.
	nnoremap S :%s//g<Left><Left>