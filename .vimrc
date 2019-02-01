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
" Plugin 'artur-shaik/vim-javacomplete2'
Plugin 'Valloric/YouCompleteMe'
Plugin 'rightson/vim-p4-syntax'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'suan/vim-instant-markdown'
Plugin 'tpope/vim-sleuth'
Plugin 'airblade/vim-gitgutter'

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

"markdown (vim-instant-markdown)
let g:instant_markdown_slow = 1

" Show whitespace
"set list
" Highlight all search results
set hlsearch
" Sensible tabs " don't need this anymore due to vim-sleuth
"set tabstop=4
"set shiftwidth=4
"set expandtab
" Mark the 80 column limit
"set colorcolumn=80
 
" show info bar
:set laststatus=2
:hi StatusLine ctermfg=blue ctermbg=red
:set ruler

" preview regex matches
:set incsearch

" git gutter: stage or undo changes
map ö>s <Plug>GitGutterStageHunk
map öu <Plug>GitGutterUndoHunk

" Highlight unwanted whitespace
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

" Highlight non ascii characters
syntax match nonascii "[^\x00-\x7F]"
highlight nonascii guibg=Blue ctermbg=2

" add highlight to nc 
autocmd BufNewFile,BufRead *.nc set filetype=nc

" run the php linter
autocmd BufWritePost *.php silent execute "!php-cs-fixer fix %" | edit! | redraw!

" enable to add debug line
au BufReadPost,BufNewFile *.py iabbrev DEBUG from pprint import pprint; import pdb; pdb.set_trace()

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

" enable backspace...
set backspace=2

" custom mappings

" NerdTree
map <C-n> :NERDTreeToggle<CR>

" Tagbar
map <C-l> :TagbarToggle<CR>

" ctags
nnoremap <C-g> <C-]>
set tags=tags;
"map <C-h> :call CurtineIncSw()<CR>

" Ctrl-P
" see https://github.com/ctrlpvim/ctrlp.vim
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']


" youcompleteme
let g:ycm_server_python_interpreter="/usr/local/bin/python3"
let g:ycm_python_binary_path = '/usr/local/bin/python3'
let g:ycm_rust_src_path = '/Users/Jan/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src'
let g:EclimCompletionMethod = 'omnifunc'
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
map <C-f> ::YcmCompleter GoTo<CR>
set splitbelow "open preview on bottom of screen"
let g:ycm_seed_identifiers_with_syntax = 1 "add language specific completions"
let g:ycm_filetype_blacklist = {
      \ 'tagbar': 1,
      \ 'qf': 1,
      \ 'notes': 1,
      \ 'markdown': 1,
      \ 'unite': 1,
      \ 'text': 1,
      \ 'vimwiki': 1,
      \ 'pandoc': 1,
      \ 'infolog': 1,
      \ 'mail': 1,
      \ 'python': 1
      \}

" java
"autocmd FileType java setlocal omnifunc=javacomplete#Complete
"nmap <C-j>I <Plug>(JavaComplete-Imports-AddMissing)
"nmap <C-j>R <Plug>(JavaComplete-Imports-RemoveUnused)
"nmap <C-j>i <Plug>(JavaComplete-Imports-AddSmart)
"nmap <C-j>ii <Plug>(JavaComplete-Imports-Add)
"
"imap <C-j>I <Plug>(JavaComplete-Imports-AddMissing)
"imap <C-j>R <Plug>(JavaComplete-Imports-RemoveUnused)
"imap <C-j>i <Plug>(JavaComplete-Imports-AddSmart)
"imap <C-j>ii <Plug>(JavaComplete-Imports-Add)
"
"nmap <C-j>M <Plug>(JavaComplete-Generate-AbstractMethods)
"
"imap <C-j>M <Plug>(JavaComplete-Generate-AbstractMethods)
"
"nmap <C-j>A <Plug>(JavaComplete-Generate-Accessors)
"nmap <C-j>s <Plug>(JavaComplete-Generate-AccessorSetter)
"nmap <C-j>g <Plug>(JavaComplete-Generate-AccessorGetter)
"nmap <C-j>a <Plug>(JavaComplete-Generate-AccessorSetterGetter)
"nmap <C-j>ts <Plug>(JavaComplete-Generate-ToString)
"nmap <C-j>eq <Plug>(JavaComplete-Generate-EqualsAndHashCode)
"nmap <C-j>c <Plug>(JavaComplete-Generate-Constructor)
"nmap <C-j>cc <Plug>(JavaComplete-Generate-DefaultConstructor)
"
"imap <C-j>s <Plug>(JavaComplete-Generate-AccessorSetter)
"imap <C-j>g <Plug>(JavaComplete-Generate-AccessorGetter)
"imap <C-j>a <Plug>(JavaComplete-Generate-AccessorSetterGetter)
"
"vmap <C-j>s <Plug>(JavaComplete-Generate-AccessorSetter)
"vmap <C-j>g <Plug>(JavaComplete-Generate-AccessorGetter)
"vmap <C-j>a <Plug>(JavaComplete-Generate-AccessorSetterGetter)
"
"nmap <silent> <buffer> <C-j>n <Plug>(JavaComplete-Generate-NewClass)
"nmap <silent> <buffer> <C-j>N <Plug>(JavaComplete-Generate-ClassInFile)
