-------------------- HELPERS -------------------------------
local cmd = vim.cmd            -- to execute Vim commands e.g. cmd('pwd')
local fn = vim.fn   	       -- to call Vim functions e.g. fn.bufnr()
local g = vim.g     	       -- a table to access global variables
local opt = vim.opt 	       -- to set options

-------------------- PLUGINS -------------------------------
require('plugins')   	       -- install plugins

-------------------- OPTIONS -------------------------------
cmd 'colorscheme gruvbox'     -- Sierra colorscheme
opt.expandtab = true           -- Use spaces instead of tabs
opt.ignorecase = true          -- Ignore case
opt.laststatus = 1             -- show status if two windows
opt.number = true              -- show line number
opt.completeopt = "menuone,noselect" 	-- enable autocompletion

-------------------- MAPPINGS ------------------------------

