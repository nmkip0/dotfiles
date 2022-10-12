(module config.init
  {autoload {nvim aniseed.nvim
             a aniseed.core
             util config.util }})

(require :config.core)
(require :config.plugins)
(require :config.keymaps)

;;(nvim.ex.colorscheme "kanagawa")
(nvim.ex.colorscheme "nord")
