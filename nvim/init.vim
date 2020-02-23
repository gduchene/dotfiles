call plug#begin(stdpath('data') . '/plugged')
  Plug 'fatih/vim-go'
call plug#end()

set expandtab
set shiftwidth=2
set tabstop=2

autocmd BufWritePre * %s/\s\+$//e
autocmd FileType go setl noet
