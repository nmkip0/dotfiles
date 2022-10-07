(module config.keymaps
  {autoload {nvim aniseed.nvim
             wk which-key
             tb telescope.builtin
             telescope telescope}})

;generic mapping leaders configuration
;(nvim.set_keymap :n :<space> :<nop> {:noremap true})
(set nvim.g.mapleader " ")
(set nvim.g.maplocalleader ",")

;clear highlighting on enter in normal mode
(nvim.set_keymap :n :<esc> ":noh<CR><CR>" {:noremap true})

;duplicate currents panel in a new tab
(nvim.set_keymap :n :<C-w>T ":tab split<CR>" {:noremap true :silent true})

;escape from terminal normal mode
(nvim.set_keymap :t :<esc><esc> "<c-\\><c-n>" {:noremap true})

(nvim.set_keymap :n :<C-h> :<C-w>h {:noremap true})
(nvim.set_keymap :n :<C-j> :<C-w>j {:noremap true})
(nvim.set_keymap :n :<C-k> :<C-w>k {:noremap true})
(nvim.set_keymap :n :<C-l> :<C-w>l {:noremap true})

(nvim.set_keymap :n :<A-Up> "<cmd>resize -2<CR>" {:noremap true})
(nvim.set_keymap :n :<A-Down> "<cmd>resize +2<CR>" {:noremap true})
(nvim.set_keymap :n :<A-Right> "<cmd>vertical resize -2<CR>" {:noremap true})
(nvim.set_keymap :n :<A-Left> "<cmd>vertical resize +2<CR>" {:noremap true})

(local keys {:b {:name "+buffer"
                 :d [":bd<cr>" "Delete buffer"]
                 :b [tb.buffers "Buffers"]
                 :n [":bnext<cr>" "Next buffer"]
                 :p [":bprevious<cr>" "Previous buffer"]}
             :f {:name "+file"
                 :f [tb.find_files "Find files"]
                 :r [tb.oldfiles "Recent files"]
                 :s [":w<cr>" "Save file"]
                 :S [":wa<cr>" "Save all files"]}
             :s {:name "+search"
                 :g [tb.live_grep "Search in dir"] }
             :o {:name "+open"
                 :p [":NvimTreeToggle<cr>" "Project"]}
             :P {:name "+packer"
                 :c [":PackerClean<cr>" "Clean"]
                 :C [":PackerCompile<cr>" "Compile"]
                 :s [":PackerSync<cr>" "Sync"]
                 :S [":PackerStatus<cr>" "Status"]}
             :/ [tb.live_grep "Search in dir"]
              })

(wk.register keys {:prefix :<leader>})

