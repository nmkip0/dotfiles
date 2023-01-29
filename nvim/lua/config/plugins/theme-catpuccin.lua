local M = {
  'catppuccin/nvim',
  name = 'catppuccin',
  lazy = false
}

function M.config()
  local cp = require 'catppuccin'
  vim.cmd('hi link user.win.title @text.note')

  cp.setup {
    flavour = "frappe", -- latte, frappe, macchiato, mocha
    background = { -- :h background
        light = "latte",
        dark = "mocha",
    },
    compile_path = vim.fn.stdpath("cache") .. "/catppuccin",
    transparent_background = false,
    term_colors = false,
    dim_inactive = {
        enabled = true,
        shade = "dark",
        percentage = 0.15,
    },
    styles = {
        comments = { "italic" },
        conditionals = { "italic" },
        loops = {},
        functions = {},
        keywords = {},
        strings = {},
        variables = {},
        numbers = {},
        booleans = {},
        properties = {},
        types = {},
        operators = {},
    },
    color_overrides = {},
    custom_highlights = {},
    integrations = {
        cmp = true,
        gitsigns = true,
        neotree = true,
        telescope = true,
        treesitter = true,
        vim_sneak = true
        -- For more plugins integrations please scroll down (https://github.com/catppuccin/nvim#integrations)
    },
}
  vim.cmd.colorscheme 'catppuccin'
end

return M
