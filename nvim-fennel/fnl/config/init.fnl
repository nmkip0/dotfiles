(module config.init
  {autoload {nvim aniseed.nvim
             a aniseed.core
             util config.util
             core config.core
             packer config.packer
             plugins config.plugins }})

(nvim.ex.colorscheme "kanagawa")

(def all-plugins (a.merge plugins.plugins))
(def all-options (a.merge core.options))

(require :config.mapping)

(packer.use! all-plugins)
(util.set-global-options! all-options)


