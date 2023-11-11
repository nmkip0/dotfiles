local keymaps = {
  {
    "<localleader>!",
    function()
      local eval = require("nmkip.lang.clojure.eval").eval
      eval("user", "(do (tap> (reset)))")
    end,
    desc = "user/reset",
  },
  {
    "<localleader>*",
    function()
      local eval = require("nmkip.lang.clojure.eval").eval
      eval("user", "(do (require '[clojure.pprint :as pprint]) (pprint/pprint *e) (tap> *e))")
    end,
    desc = "Eval last error",
  },
}

return {
  {
    "Olical/conjure",
    version = "4.*",
    ft = { "clojure", "lua" },
    keys = keymaps,
    init = function()
      vim.g["conjure#highlight#enabled"] = true
      vim.g["conjure#highlight#group"] = "CurrentWord"
      vim.g["conjure#highlight#timeout"] = 150
      vim.g["conjure#eval#inline#highlight"] = "CurrentWord"

      vim.g["conjure#extract#tree_sitter#enabled"] = true

      vim.g["conjure#client#clojure#nrepl#eval#raw_out"] = true
      vim.g["conjure#log#hud#enabled"] = false
      vim.g["conjure#log#wrap"] = true
      vim.g["conjure#client#clojure#nrepl#connection#auto_repl#enabled"] = false
      vim.g["conjure#client#clojure#nrepl#eval#auto_require"] = false

      vim.g["conjure#mapping#doc_word"] = false

      vim.g["conjure#client#clojure#nrepl#mapping#disconnect"] = false
      vim.g["conjure#client#clojure#nrepl#mapping#connect_port_file"] = false

      vim.g["conjure#client#clojure#nrepl#mapping#session_clone"] = false
      vim.g["conjure#client#clojure#nrepl#mapping#session_fresh"] = false
      vim.g["conjure#client#clojure#nrepl#mapping#session_close"] = false
      vim.g["conjure#client#clojure#nrepl#mapping#session_close_all"] = false
      vim.g["conjure#client#clojure#nrepl#mapping#session_list"] = false
      vim.g["conjure#client#clojure#nrepl#mapping#session_next"] = false
      vim.g["conjure#client#clojure#nrepl#mapping#session_prev"] = false
      vim.g["conjure#client#clojure#nrepl#mapping#session_select"] = false

      vim.g["conjure#client#clojure#nrepl#mapping#refresh_changed"] = false
      vim.g["conjure#client#clojure#nrepl#mapping#refresh_all"] = false
      vim.g["conjure#client#clojure#nrepl#mapping#refresh_clear"] = false


      -- Disable default log functions
      vim.g["conjure#mapping#log_split"] = false
      vim.g["conjure#mapping#log_vsplit"] = false
      vim.g["conjure#mapping#log_toggle"] = false

      -- Disable default evals
      vim.g["conjure#mapping#eval_current_form"] = false
      vim.g["conjure#mapping#eval_root_form"] = false
      vim.g["conjure#mapping#eval_word"] = false

      vim.g["conjure#log#jump_to_latest#cursor_scroll_position"] = "none"
    end,
    config = function()
      local wk = require("which-key")

      local nrepl = require("nmkip.lang.clojure.nrepl")
      local action = require("conjure.client.clojure.nrepl.action")
      local server = require("conjure.client.clojure.nrepl.server")

      wk.register({
        c = {
          name = "+connect",

          f = {
            nrepl.find_repls,
            "Find and connect to running repls",
          },
          c = { action["connect-port-file"], "Connect via port file" },
          d = { server["disconnect"], "Disconnect" },
          p = { nrepl.direct_connect, "Connect via port" },

          l = {
            function()
              vim.cmd("ConjureLogToggle")
              vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-W>p", true, true, true), "n", true)
            end,
            "Open the repl log buffer",
          },
        }
      }, {
        prefix = "<localleader>",
      })

      vim.api.nvim_create_autocmd("FileType", {
        pattern = "clojure",
        callback = function(event)
          wk.register({
            e = {
              name = "+eval",
              w = { require("nmkip.lang.clojure.eval").eval_tap_word, "Eval word" },
              e = { require("nmkip.lang.clojure.eval").eval_tap_form, "Eval form" },
              f = { require("nmkip.lang.clojure.eval").eval_tap_form, "Eval form" },
              r = { require("nmkip.lang.clojure.eval").eval_tap_root_form, "Eval root form" },
            },


            l = {
              name = "+log",
              g = { require("nmkip.lang.clojure.log").toggle, "Toggle" },
              s = { require("nmkip.lang.clojure.log").open_horizontal_split, "Open in horizontal window" },
              v = { require("nmkip.lang.clojure.log").open_vertical_split, "Open in vertical window" },
            },

            n = {
              name = "+namespace",
              r = { action["refresh-changed"], "Refresh changed namespaces" },
              R = { action["refresh-all"], "Refresh all namespaces" },
            },

            t = {
              name = "+test"
            },

            w = {
              name = "+wrap"
            },

            S = {
              require("nmkip.lang.clojure.defsc").defsc,
              "def score capture",
            }

          }, {
            prefix = "<localleader>",
            buffer = event.buf,
          })
        end,
      })

      vim.api.nvim_create_autocmd("BufNewFile", {
        pattern = "conjure-log-*",
        desc = "Disable diagnostics in conjure log buffer",
        callback = function(event)
          vim.diagnostic.disable(event.buf)
        end,
      })
    end,
  },
}
