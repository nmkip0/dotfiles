(module config.init
  {autoload {nvim aniseed.nvim
             a aniseed.core
             util config.util
             core config.core
             packer config.packer
             plugins config.plugins }})

(require :config.keymaps)

(nvim.ex.colorscheme "kanagawa")

(util.set-global-options! core.options)
