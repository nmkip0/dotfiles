local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP = fn.system {
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  }
  print "Installing packer close and reopen Neovim..."
  vim.cmd [[packadd packer.nvim]]
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]]

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
  return
end

-- Have packer use a popup window
packer.init {
  display = {
    open_fn = function()
      return require("packer.util").float { border = "rounded" }
    end,
  },
}

-- Install your plugins here
return packer.startup(function(use)
  -- My plugins here
  use "wbthomason/packer.nvim" -- Have packer manage itself
  use "nvim-lua/popup.nvim" -- An implementation of the Popup API from vim in Neovim
  use "nvim-lua/plenary.nvim" -- Useful lua functions used ny lots of plugins
  use "numToStr/Comment.nvim" -- Easily comment stuff
  use "lewis6991/impatient.nvim"
  use "moll/vim-bbye" -- Bbye allows you to do delete buffers without closing your windows or messing up your layout.
  use "windwp/nvim-autopairs"

  -- session
  use 'rmagatti/auto-session'

  -- windows
  use 'sindrets/winshift.nvim'
  use 'declancm/maximize.nvim'
  use {'s1n7ax/nvim-window-picker', tag = "v1.*"}

  -- themes
  use 'shaunsingh/nord.nvim'
  use "EdenEast/nightfox.nvim"
  use {"catppuccin/nvim", as = "catppuccin"}

  use "kyazdani42/nvim-web-devicons"
  --[[ use "kyazdani42/nvim-tree.lua" ]]
  use {
  "nvim-neo-tree/neo-tree.nvim",
    branch = "v2.x",
    requires = {
      "MunifTanjim/nui.nvim",
    }
  }
  use "nvim-lualine/lualine.nvim"

  use {
    "Olical/conjure",
    branch = "develop"
  }

  use "guns/vim-sexp"
  use "kylechui/nvim-surround"
  use "radenling/vim-dispatch-neovim"
  use "tpope/vim-repeat"

  use 'justinmk/vim-sneak'

  -- cmp plugins
  use "hrsh7th/nvim-cmp" -- The completion plugin
  use "hrsh7th/cmp-buffer" -- buffer completions
  use "hrsh7th/cmp-path" -- path completions
  use "hrsh7th/cmp-cmdline" -- cmdline completions
  use "saadparwaiz1/cmp_luasnip" -- snippet completions
  use "hrsh7th/cmp-nvim-lsp" -- lsp completions
  use "hrsh7th/cmp-nvim-lua" -- lua completions
  use "PaterJason/cmp-conjure" -- lua completions

  -- snippets
  use "L3MON4D3/LuaSnip" --snippet engine
  use "rafamadriz/friendly-snippets" -- a bunch of snippets to use

  -- LSP
  use "neovim/nvim-lspconfig" -- enable LSP
  use "williamboman/nvim-lsp-installer"  -- simple to use language server installer
  use "lukas-reineke/lsp-format.nvim"
  use "folke/neodev.nvim"

  -- Telescope
  use "nvim-telescope/telescope.nvim"
  use 'nvim-telescope/telescope-ui-select.nvim'
  use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }

  -- Treesitter
  use {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
  }
  use "p00f/nvim-ts-rainbow"
  use "nvim-treesitter/playground"
  use 'nvim-treesitter/nvim-treesitter-textobjects'
  use 'nvim-treesitter/nvim-treesitter-context'
  use 'JoosepAlviste/nvim-ts-context-commentstring'

  -- Git
  use "lewis6991/gitsigns.nvim"

  use "folke/which-key.nvim"
  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if PACKER_BOOTSTRAP then
    require("packer").sync()
  end
end)
