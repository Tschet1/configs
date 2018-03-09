set nocp
execute pathogen#infect()

"set nocompatible              " be iMproved, required
"filetype off                  " required
 
" set the runtime path to include Vundle and initialize
"set rtp+=~/.vim/bundle/Vundle.vim
"call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')
 
" let Vundle manage Vundle, required
"Plugin 'VundleVim/Vundle.vim'
 
"Plugin 'vim-airline/vim-airline'                                              
"Plugin 'vim-airline/vim-airline-themes'                         
"let g:airline_powerline_fonts = 1
"let g:airline_theme = 'solarized'
"let g:airline_solarized_bg = 'dark'
 
" All of your Plugins must be added before the following line
"call vundle#end()            " required
filetype plugin indent on    " required
 
:syntax enable
 
" use same whitespaces as the last line
set copyindent
 
"enable paste mode
set pastetoggle=
 
"use tabs in makefiles
autocmd FileType make setlocal noexpandtab
 
" Show whitespace
"set list
" Highlight all search results
set hlsearch
" Sensible tabs
set tabstop=4
set shiftwidth=4
set expandtab
" Mark the 80 column limit
"set colorcolumn=80
 
" show info bar
:set laststatus=2
:hi StatusLine ctermfg=blue ctermbg=red
:set ruler
 
" Highlight unwanted whitespace
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

" add highlight to nc 
autocmd BufNewFile,BufRead *.nc set filetype=nc

" run the php linter
autocmd BufWritePost *.php silent execute "!php-cs-fixer fix %" | edit! | redraw!

" add nesc to tagbar
let g:tagbar_type_nc = {
    \ 'ctagstype': 'nc',
    \ 'kinds' : [
        \'b:configuration',
        \'c:command',
        \'d:definition',
        \'e:event',
        \'f:function',
        \'i:interface',
        \'m:module',
        \'t:task',
        \'r:result',
        \'u:uses'
    \ ]
\ }

" custom mappings
map <C-n> :NERDTreeToggle<CR>
map <C-l> :TagbarToggle<CR>
nnoremap <C-g> <C-]>
map <C-h> :call CurtineIncSw()<CR>
