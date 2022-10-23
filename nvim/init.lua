-- Entrypoint for my Neovim configuration

require "user.options"
require "user.keymaps"
require "user.plugins"
require "user.colorscheme"

require "user.cmp"
require "user.lsp" --when requiring dirs lua looks for init.lua

require "user.telescope"
require "user.treesitter"
--[[ require "user.nvim-tree" ]]
require "user.window-picker"
require "user.neo-tree"
require "user.lualine"
require "user.comment"
require "user.gitsigns"
require "user.which-key"
require "user.impatient"

require "user.sexp"
require "user.surround"
require "user.conjure"
