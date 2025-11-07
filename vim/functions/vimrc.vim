" Vim configuration and scripting examples

" Basic settings
set nocompatible              " Disable Vi compatibility
syntax on                     " Enable syntax highlighting
filetype plugin indent on     " Enable file type detection

" Display settings
set number                    " Show line numbers
set relativenumber           " Show relative line numbers
set cursorline               " Highlight current line
set showmatch                " Show matching brackets
set showcmd                  " Show command in status line
set ruler                    " Show cursor position
set laststatus=2             " Always show status line

" Indentation
set tabstop=4                " Tab width
set shiftwidth=4             " Indent width
set expandtab                " Use spaces instead of tabs
set autoindent               " Auto indent
set smartindent              " Smart indent

" Search settings
set hlsearch                 " Highlight search results
set incsearch                " Incremental search
set ignorecase               " Case insensitive search
set smartcase                " Case sensitive if uppercase present

" Editor behavior
set backspace=indent,eol,start  " Allow backspace over everything
set wrap                        " Wrap long lines
set linebreak                   " Break lines at word boundaries
set scrolloff=5                 " Keep 5 lines above/below cursor
set mouse=a                     " Enable mouse support

" File handling
set encoding=utf-8
set fileencoding=utf-8
set autoread                 " Auto reload changed files
set hidden                   " Allow hidden buffers

" Backup and undo
set nobackup
set nowritebackup
set noswapfile
set undofile
set undodir=~/.vim/undo

" Variables
let g:author = "John Doe"
let g:email = "john@example.com"
let mapleader = ","          " Set leader key

" String variables
let message = "Hello, Vim!"
let path = expand("~/.vim")

" Lists
let colors = ['red', 'green', 'blue']
let numbers = [1, 2, 3, 4, 5]

" Dictionaries
let person = {
    \ 'name': 'Alice',
    \ 'age': 30,
    \ 'email': 'alice@example.com'
    \ }

" Functions
function! Greet(name)
    echo "Hello, " . a:name . "!"
endfunction

call Greet("World")

" Function with return value
function! Add(x, y)
    return a:x + a:y
endfunction

let sum = Add(5, 3)
echo "Sum: " . sum

" Function with optional arguments
function! OptionalArgs(required, ...)
    echo "Required: " . a:required
    if a:0 > 0
        echo "Optional 1: " . a:1
    endif
    if a:0 > 1
        echo "Optional 2: " . a:2
    endif
endfunction

" Range function (operates on lines)
function! CommentLines() range
    for linenum in range(a:firstline, a:lastline)
        let line = getline(linenum)
        call setline(linenum, '" ' . line)
    endfor
endfunction

" Conditionals
if has('python3')
    echo "Python 3 support available"
elseif has('python')
    echo "Python 2 support available"
else
    echo "No Python support"
endif

" Ternary operator
let result = (sum > 5) ? "Greater" : "Less"

" Loops
" For loop
for item in colors
    echo item
endfor

" For loop with index
for i in range(5)
    echo "Number: " . i
endfor

" While loop
let counter = 0
while counter < 5
    echo "Counter: " . counter
    let counter += 1
endwhile

" Key mappings
" Normal mode mappings
nnoremap <leader>w :w<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader>/ :noh<CR>

" Insert mode mappings
inoremap jk <Esc>

" Visual mode mappings
vnoremap < <gv
vnoremap > >gv

" Map function key
map <F5> :!python3 %<CR>

" Autocommands
augroup FileTypeSettings
    autocmd!
    autocmd FileType python setlocal tabstop=4 shiftwidth=4
    autocmd FileType javascript setlocal tabstop=2 shiftwidth=2
    autocmd FileType html setlocal tabstop=2 shiftwidth=2
augroup END

" Auto-save on focus lost
autocmd FocusLost * silent! wa

" Jump to last cursor position
autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

" Commands
" Custom command
command! MakeTags !ctags -R .
command! JsonFormat %!python3 -m json.tool
command! -nargs=1 GitCommit !git commit -m "<args>"

" Command with completion
command! -nargs=1 -complete=file Edit edit <args>

" Abbreviations
iabbrev @@ john@example.com
iabbrev teh the
iabbrev waht what

" String operations
let text = "hello world"
let upper = toupper(text)
let lower = tolower(text)
let length = len(text)
let substr = text[0:4]

" List operations
call add(numbers, 6)
let first = numbers[0]
let last = numbers[-1]
let sliced = numbers[1:3]
call remove(numbers, 0)
call insert(numbers, 0, 0)

" Dictionary operations
let person.city = "NYC"
call remove(person, 'age')
let keys = keys(person)
let values = values(person)
let has_name = has_key(person, 'name')

" File operations
if filereadable(expand("~/.vimrc.local"))
    source ~/.vimrc.local
endif

" Check if directory exists
if !isdirectory(expand("~/.vim/undo"))
    call mkdir(expand("~/.vim/undo"), "p")
endif

" Regular expressions
let matched = match("hello world", "world")
let substituted = substitute("hello world", "world", "vim", "")

" System commands
let git_branch = system("git branch --show-current")
let git_status = systemlist("git status --short")

" Plugins (using vim-plug)
" call plug#begin('~/.vim/plugged')
" Plug 'tpope/vim-surround'
" Plug 'junegunn/fzf.vim'
" Plug 'preservim/nerdtree'
" call plug#end()

" Color scheme
try
    colorscheme desert
catch
    colorscheme default
endtry

" Highlighting
highlight Normal guibg=NONE ctermbg=NONE
highlight Comment ctermfg=gray

" Status line
set statusline=%f         " File path
set statusline+=%m        " Modified flag
set statusline+=%r        " Read only flag
set statusline+=%=        " Right align
set statusline+=%l/%L     " Line number
set statusline+=\ %p%%    " Percentage
set statusline+=\ %y      " File type

" Script-local variables
let s:local_var = "Private"

" Script-local function
function! s:HelperFunction()
    echo "This is private"
endfunction

" Execute Ex command
execute "set number"
execute "echo 'Hello'"

" Eval expression
let result = eval("2 + 3")

" Try-catch
try
    call NonExistentFunction()
catch /^Vim\%((\a\+)\)\=:E/
    echo "Caught error: " . v:exception
finally
    echo "Cleanup"
endtry

" Lambda functions (Vim 8+)
let Square = {x -> x * x}
echo Square(5)

let Filter = {list -> filter(copy(list), 'v:val % 2 == 0')}
echo Filter([1,2,3,4,5,6])

" Timers (Vim 8+)
function! TimerCallback(timer)
    echo "Timer fired!"
endfunction

" let timer_id = timer_start(1000, 'TimerCallback')

" Jobs and channels (Vim 8+)
" let job = job_start(['python3', 'script.py'])

" Terminal (Vim 8+)
" :terminal

echo "Vimscript examples complete!"
