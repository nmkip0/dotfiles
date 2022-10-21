-- Entrypoint for my Neovim configuration

require "user.options"
require "user.keymaps"
require "user.plugins"
require "user.colorscheme"

require "user.cmp"
require "user.lsp" --when requiring dirs lua looks for init.lua

require "user.telescope"