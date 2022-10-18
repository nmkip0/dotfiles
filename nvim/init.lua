-- Entrypoint for my Neovim configuration

require "user.options"
require "user.keymaps"
require "user.plugins"

require "user.cmp"
require "user.lsp" --when requiring dirs lua looks for init.lua

