-- make configuration to look like in vim script
function bind (mode, keys, command, options)
	opts = { noremap = true }
	if options then
		opts = options
	end

	vim.keymap.set(mode, keys, command, opts)

end

set = vim.opt
let = vim.g

-- line numbers
set.nu = true
set.relativenumber = true

-- tabs
set.smartindent = true
set.autoindent = true
set.smarttab = true
set.tabstop = 4
set.shiftwidth = 4
set.softtabstop = 4

-- make neovim not suck
set.encoding = 'UTF-8'
set.clipboard = "unnamedplus"
set.undofile = true
set.syntax = on

-- open file on recent line
vim.api.nvim_create_autocmd("BufReadPost", {
    pattern = {"*"},
    callback = function()
        if vim.fn.line("'\"") > 1 and vim.fn.line("'\"") <= vim.fn.line("$") then
            vim.api.nvim_exec("normal! g'\"",false)
        end
    end
})

-- keybindings

-- go to the end of line
bind('n', '<C-e>', '$')

-- enable or disable relativenumber
bind('n', '<C-n>', ':set relativenumber <CR>')
bind('n', '<C-n>', ':set relativenumber! <CR>')

-- easy quit from nvim with save
bind('n', 'Q', ':wq <CR>')

-- quit from nvim without save
bind('n', '<C-q>', ':q! <CR>')

-- leader key keybindings
let.mapleader = ';'

-- execute files
bind('n', '<leader>ep', ':!python3 % <CR>')
bind('n', '<leader>es', ':!sh % <CR>')
bind('n', '<leader>ec', ':!gcc -o a % && ./a <CR>')

-- split
local split = function(direction)
   if direction == 'horizontal' then
	  vim.api.nvim_command('split')
   elseif direction == 'vertical' then
	  vim.api.nvim_command('vsplit')
   end

   vim.api.nvim_command('wincmd w')
end

local splitVertical = function()
   split('vertical')
end

local splitHorizontal = function()
   split('horizontal')
end

bind('n', '<leader>w', splitVertical)
bind('n', '<leader>W', splitHorizontal)

-- open file
bind('n', '<leader>of', ':e ')

-- create new file
bind('n', '<leader>nf', ':!touch ')

-- create new dir
bind('n', '<leader>nd', ':!mkdir ')
