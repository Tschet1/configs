set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'scrooloose/nerdtree'
Plugin 'vim-syntastic/syntastic'
Plugin 'majutsushi/tagbar'
Plugin 'artur-shaik/vim-javacomplete2'
Plugin 'Tschet1/CurtineIncSw.vim'
Plugin 'Valloric/YouCompleteMe'

" All of your Plugins must be added before the following line
call vundle#end()            " required
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

" youcompleteme
let g:ycm_server_python_interpreter="/usr/bin/python"
let g:ycm_python_binary_path = '/usr/local/bin/python'

" java
autocmd FileType java setlocal omnifunc=javacomplete#Complete
nmap <C-j>I <Plug>(JavaComplete-Imports-AddMissing)
nmap <C-j>R <Plug>(JavaComplete-Imports-RemoveUnused)
nmap <C-j>i <Plug>(JavaComplete-Imports-AddSmart)
nmap <C-j>ii <Plug>(JavaComplete-Imports-Add)

imap <C-j>I <Plug>(JavaComplete-Imports-AddMissing)
imap <C-j>R <Plug>(JavaComplete-Imports-RemoveUnused)
imap <C-j>i <Plug>(JavaComplete-Imports-AddSmart)
imap <C-j>ii <Plug>(JavaComplete-Imports-Add)

nmap <C-j>M <Plug>(JavaComplete-Generate-AbstractMethods)

imap <C-j>M <Plug>(JavaComplete-Generate-AbstractMethods)

nmap <C-j>A <Plug>(JavaComplete-Generate-Accessors)
nmap <C-j>s <Plug>(JavaComplete-Generate-AccessorSetter)
nmap <C-j>g <Plug>(JavaComplete-Generate-AccessorGetter)
nmap <C-j>a <Plug>(JavaComplete-Generate-AccessorSetterGetter)
nmap <C-j>ts <Plug>(JavaComplete-Generate-ToString)
nmap <C-j>eq <Plug>(JavaComplete-Generate-EqualsAndHashCode)
nmap <C-j>c <Plug>(JavaComplete-Generate-Constructor)
nmap <C-j>cc <Plug>(JavaComplete-Generate-DefaultConstructor)

imap <C-j>s <Plug>(JavaComplete-Generate-AccessorSetter)
imap <C-j>g <Plug>(JavaComplete-Generate-AccessorGetter)
imap <C-j>a <Plug>(JavaComplete-Generate-AccessorSetterGetter)

vmap <C-j>s <Plug>(JavaComplete-Generate-AccessorSetter)
vmap <C-j>g <Plug>(JavaComplete-Generate-AccessorGetter)
vmap <C-j>a <Plug>(JavaComplete-Generate-AccessorSetterGetter)

nmap <silent> <buffer> <C-j>n <Plug>(JavaComplete-Generate-NewClass)
nmap <silent> <buffer> <C-j>N <Plug>(JavaComplete-Generate-ClassInFile)
