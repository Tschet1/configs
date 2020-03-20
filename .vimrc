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
" Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'suan/vim-instant-markdown'
Plugin 'tpope/vim-sleuth'
Plugin 'airblade/vim-gitgutter'
Plugin 'neomake/neomake'
Plugin 'cpiger/NeoDebug'
Plugin 'Vimjas/vim-python-pep8-indent'
Plugin 'tpope/vim-fugitive'
Plugin 'junegunn/fzf'
Plugin 'junegunn/FZF.vim'

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

" pep8 intend for python
python_pep8_indent_hang_closing = 0

" syntax check on read and write
function! MyOnBattery()
  if has('macunix')
    return match(system('pmset -g batt'), "Now drawing from 'Battery Power'") != -1
  elsif has('unix')
    return readfile('/sys/class/power_supply/AC/online') == ['0']
  endif
  return 0
endfunction

if MyOnBattery()
  call neomake#configure#automake('rw')
else
  call neomake#configure#automake('nw', 1000)
  set updatetime=1000
endif

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
map ps <Plug>GitGutterStageHunk
map pu <Plug>GitGutterUndoHunk

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

" enable to add C style comment
au BufReadPost,BufNewFile *.[ch] iabbrev COMMENT /* */<Left><Left><Left>

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
map <C-b> :tn<CR>
set tags=tags;
"map <C-h> :call CurtineIncSw()<CR>

" Ctrl-P
" see https://github.com/ctrlpvim/ctrlp.vim
" let g:ctrlp_map = '<c-p>'
" let g:ctrlp_cmd = 'CtrlP'
" let g:ctrlp_working_path_mode = 'ra'
" let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

" fzf
map <C-p> :Files <CR>

" --column: Show column number
" --line-number: Show line number
" --no-heading: Do not show file headings in results
" --fixed-strings: Search term as a literal string
" --ignore-case: Case insensitive search
" --no-ignore: Do not respect .gitignore, etc...
" --hidden: Search hidden files and folders
" --follow: Follow symlinks
" --glob: Additional conditions for search (in this case ignore everything in the .git/ folder)
" --color: Search color options
command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>), 1, <bang>0)
map <C-h> :Find <C-r><C-w><CR>
map <C-y> :Find 

" youcompleteme
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
map <C-f> ::YcmCompleter GoTo<CR>
map <C-s> ::YcmCompleter GetDoc<CR>
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
      \ 'mail': 1
      \}
"     \ 'python': 1

" color
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
colorscheme slate

" folding
"set nofoldenable
set foldmethod=syntax
set foldnestmax=1
set foldlevel=99

" NeoDebug
map <C-x>x ::NeoDebug<CR>
let g:neodbg_keymap_toggle_breakpoint  = '<C-x>b'      " toggle breakpoint on current line
let g:neodbg_keymap_next               = '<F3>'        " next
let g:neodbg_keymap_run_to_cursor      = '<S-F2>'      " run to cursor (tb and c)
let g:neodbg_keymap_jump               = '<S-F3>'    " set next statement (tb and jump)
let g:neodbg_keymap_step_into          = '<F4>'        " step into
let g:neodbg_keymap_step_out           = '<S-F4>'      " setp out
let g:neodbg_keymap_continue           = '<F2>'         " run or continue
let g:neodbg_keymap_print_variable     = '<C-x>p'        " view variable under the cursor
let g:neodbg_keymap_stop_debugging     = '<C-x>e'       " stop debugging (kill)
let g:neodbg_keymap_toggle_console_win = '<F6>'         " toggle console window
let g:neodbg_keymap_terminate_debugger = '<C-x>x'        " terminate debugger

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
