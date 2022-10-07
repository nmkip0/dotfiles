(module config.plugins
  {autoload {packer config.packer}})

(def plugins 
  {
   ;; plugin Manager
   :wbthomason/packer.nvim {}
   ;; nvim config and plugins in Fennel
   :Olical/aniseed {:branch :develop}

   :tpope/vim-repeat {}

   ;; git helper
   :lewis6991/gitsigns.nvim {:mod :gitsigns}

   ;; themes
   :folke/tokyonight.nvim {}
   ;;:navarasu/onedark.nvim {:mod :onedark}
   :rebelot/kanagawa.nvim {}
   :NTBBloodbath/doom-one.nvim {}
   ;; icons
   :ryanoasis/vim-devicons {}
   :kyazdani42/nvim-web-devicons {}

   ;; status line
   :nvim-lualine/lualine.nvim {:mod :lualine
                               :require [:kyazdani42/nvim-web-devicons]}

   ;; file searching
   :nvim-telescope/telescope-fzf-native.nvim {:run :make}
   :nvim-telescope/telescope.nvim {:requires [:nvim-telescope/telescope-ui-select.nvim
                                              :nvim-lua/popup.nvim
                                              :jvgrootveld/telescope-zoxide
                                              :nvim-lua/plenary.nvim
                                              :nvim-telescope/telescope-fzf-native.nvim]
                                   :mod :telescope}

   ;; commeting code
   :numToStr/Comment.nvim {:mod :comment}

   :tpope/vim-surround {}

   ;; repl tools
   :Olical/conjure {:branch :develop
                    :mod :conjure
                    :requires [:guns/vim-sexp ]}

   ; sexp
   :guns/vim-sexp {:mod :sexp}

   ;; which-key
   :folke/which-key.nvim {:mod :which-key}

   ;; parsing system
   :nvim-treesitter/nvim-treesitter {:run ":TSUpdate"
                                     :mod :treesitter}


   ;; nvim-tree
   :kyazdani42/nvim-tree.lua {:mod :nvim-tree
                              :requires [:kyazdani42/nvim-web-devicons]}

   :ahmedkhalf/project.nvim {:mod :project}

   ; lsp
   :neovim/nvim-lspconfig {:mod :lspconfig}

   ; luasnip
   :L3MON4D3/LuaSnip {:requires [:saadparwaiz1/cmp_luasnip]}

   ; autocomplete
   :hrsh7th/nvim-cmp {:requires [:hrsh7th/cmp-buffer
                                 :hrsh7th/cmp-path
                                 :hrsh7th/cmp-calc
                                 :hrsh7th/cmp-nvim-lsp
                                 :hrsh7th/cmp-nvim-lua
                                 :hrsh7th/cmp-vsnip
                                 :PaterJason/cmp-conjure]
                      :mod :cmp}


   :radenling/vim-dispatch-neovim {:requires [:tpope/vim-dispatch]}

   ; toggleterm
   :akinsho/toggleterm.nvim {}

   ; clip
   ;;  :AckslD/nvim-neoclip.lua 
   ;;  {:mod :neoclip
   ;;   :requires [:nvim-telescope/telescope.nvim
   ;;              {1 :kkharji/sqlite.lua :module :sqlite}]}

   ; multicursor selector
   ;; :mg979/vim-visual-multi {}

   ; text alignment
   ;; :junegunn/vim-easy-align {:mod :easy-align}
   ;; there's a lua version


   ;;  :kylechui/nvim-surround {:mod :surround}

;;  :tpope/vim-abolish {}
;;  :tpope/vim-eunuch {}
;;  :tpope/vim-sleuth {}

;;  :tpope/vim-unimpaired {}
;;  :tpope/vim-vinegar {} 

;;
; colorizer
;;  :norcalli/nvim-colorizer.lua {:mod :colorizer}

; hop
;;  :phaazon/hop.nvim {:mod :hop :branch "v2"}
})

(packer.use! plugins)
