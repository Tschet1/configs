set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=/usr/local/opt/fzf
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'scrooloose/nerdtree'
Plugin 'vim-syntastic/syntastic'
Plugin 'majutsushi/tagbar'
" Plugin 'artur-shaik/vim-javacomplete2'
" Plugin 'Valloric/YouCompleteMe'
Plugin 'rightson/vim-p4-syntax'
" Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'suan/vim-instant-markdown'
Plugin 'tpope/vim-sleuth'
Plugin 'airblade/vim-gitgutter'
Plugin 'lumiliet/vim-twig'
Plugin 'neomake/neomake'
Plugin 'cpiger/NeoDebug'
Plugin 'Vimjas/vim-python-pep8-indent'
Plugin 'tpope/vim-fugitive'
Plugin 'junegunn/fzf'
Plugin 'junegunn/FZF.vim'
Plugin 'vim-scripts/DoxygenToolkit.vim'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'gu-fan/riv.vim'
Plugin 'Rykka/InstantRst'
Plugin 'tpope/vim-commentary'
Plugin 'numirias/semshi'
Plugin 'neoclide/coc.nvim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
 
:syntax enable

" use same whitespaces as the last line
set copyindent
 
"enable paste mode
set pastetoggle=

"case insensitive search
set ignorecase

"use tabs in makefiles
autocmd FileType make setlocal noexpandtab

"markdown (vim-instant-markdown)
let g:instant_markdown_slow = 1

" pep8 intend for python
" python_pep8_indent_hang_closing = 0

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
 
" show info bar
:set laststatus=2
:hi StatusLine ctermfg=blue ctermbg=red
:set ruler

" preview regex matches
:set incsearch

" git gutter: stage or undo changes
map ps <Plug>(GitGutterStageHunk)
map pu <Plug>(GitGutterUndoHunk)

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
"let g:ycm_server_python_interpreter="/usr/local/bin/python3"
"let g:ycm_python_binary_path = '/usr/local/bin/python3'
"let g:ycm_rust_src_path = '/Users/Jan/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src'
"let g:ycm_autoclose_preview_window_after_completion = 1
"let g:ycm_autoclose_preview_window_after_insertion = 1
"map <C-f> ::YcmCompleter GoTo<CR>
"map <C-s> ::YcmCompleter GetDoc<CR>
"set splitbelow "open preview on bottom of screen"
"let g:ycm_seed_identifiers_with_syntax = 1 "add language specific completions"
"let g:ycm_filetype_blacklist = {
"      \ 'tagbar': 1,
"      \ 'qf': 1,
"      \ 'notes': 1,
"      \ 'markdown': 1,
"      \ 'unite': 1,
"      \ 'text': 1,
"      \ 'vimwiki': 1,
"      \ 'pandoc': 1,
"      \ 'infolog': 1,
"      \ 'mail': 1
"      \}
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




""""""""""" COC """"""""
" TextEdit might fail if hidden is not set.
set hidden

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=1

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Remap <C-f> and <C-b> for scroll float windows/popups.
" Note coc#float#scroll works on neovim >= 0.4.0 or vim >= 8.2.0750
nnoremap <nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
nnoremap <nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
inoremap <nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
inoremap <nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"

" NeoVim-only mapping for visual mode scroll
" Useful on signatureHelp after jump placeholder of snippet expansion
if has('nvim')
  vnoremap <nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#nvim_scroll(1, 1) : "\<C-f>"
  vnoremap <nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#nvim_scroll(0, 1) : "\<C-b>"
endif

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline=
set statusline+=%F%{&modified?'[+]':''}
set statusline+=%=%{coc#status()}%{get(b:,'coc_current_function','')}
set statusline+=%=%l/%L,%v\ %p%%

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>

"""""""""" END COC """""""""

map <C-m> :% s/<C-r><C-w>/<C-r><C-w>/g

" Don't mark the 80 column limit
set colorcolumn=
