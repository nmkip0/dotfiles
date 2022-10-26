local configure_nord = function()
  local status_ok, nord = pcall(require, "nord")
  if not status_ok then
    return status_ok
  end
  vim.api.nvim_command "colorscheme nord"
  vim.g.nord_contrast = true
  vim.g.nord_borders = true
  vim.g.nord_disable_background = false
  vim.g.nord_italic = false
  vim.g.nord_uniform_diff_background = true

  nord.set()
end

local configure_catppuccin = function()
  local status_ok, catpuccin = pcall(require, "catppuccin")
  if not status_ok then
    return
  end
  
  catpuccin.setup({
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
        -- For more plugins integrations please scroll down (https://github.com/catppuccin/nvim#integrations)
    },
})

  vim.api.nvim_command "colorscheme catppuccin"
end

local status_ok = pcall(configure_catppuccin)
if not status_ok then
  vim.api.nvim_command "colorscheme default"
  vim.api.nvim_command "set background=dark"
end
