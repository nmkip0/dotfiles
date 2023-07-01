local M = {
  "Olical/conjure",
  branch = "develop",
  ft = { "clojure", "lua" },
}

M.config = function()
  vim.g["conjure#extract#tree_sitter#enabled"] = true

  vim.g["conjure#highlight#enabled"] = true
  vim.g["conjure#highlight#timeout"] = 150

  vim.g["conjure#client#clojure#nrepl#connection#auto_repl#enabled"] = false
  vim.g["conjure#client#clojure#nrepl#eval#raw_out"] = true
  vim.g["conjure#client#clojure#nrepl#test#raw_out"] = true
  vim.g["conjure#client#clojure#nrepl#test#runner"] = "kaocha"

  vim.g["conjure#log#wrap"] = true
  vim.g["conjure#log#jump_to_latest#enabled"] = true
  vim.g["conjure#log#jump_to_latest#cursor_scroll_position"] = "center"
  vim.g["conjure#log#hud#enabled"] = false

  -- disable mappings
  vim.g["conjure#client#clojure#nrepl#mapping#session_clone"] = false
  vim.g["conjure#client#clojure#nrepl#mapping#session_fresh"] = false
  vim.g["conjure#client#clojure#nrepl#mapping#session_close"] = false
  vim.g["conjure#client#clojure#nrepl#mapping#session_close_all"] = false
  vim.g["conjure#client#clojure#nrepl#mapping#session_list"] = false
  vim.g["conjure#client#clojure#nrepl#mapping#session_next"] = false
  vim.g["conjure#client#clojure#nrepl#mapping#session_prev"] = false
  vim.g["conjure#client#clojure#nrepl#mapping#session_select"] = false

  vim.g["conjure#mapping#log_split"] = false
  vim.g["conjure#mapping#log_vsplit"] = false
  vim.g["conjure#mapping#log_toggle"] = false
  vim.g["conjure#mapping#eval_current_form"] = false
  -- vim.g["conjure#mapping#eval_comment_current_form"] = true
  vim.g["conjure#mapping#eval_root_form"] = false
  -- vim.g["conjure#mapping#eval_comment_root_form"] = true
  vim.g["conjure#mapping#eval_word"] = false
  -- vim.g["conjure#mapping#eval_motion"] = true
  -- vim.g["conjure#mapping#eval_buf"] = true
  -- vim.g["conjure#mapping#eval_visual"] = true
  -- vim.g["conjure#mapping#eval_file"] = true
  -- vim.g["conjure#mapping#eval_replace_form"] = true
  -- vim.g["conjure#mapping#eval_comment_word"] = true

  vim.g["conjure#mapping#doc_word"] = false
  vim.g["conjure#mapping#def_word"] = false

  local grp = vim.api.nvim_create_augroup("conjure_hooks", { clear = true })
  vim.api.nvim_create_autocmd("BufNewFile", {
    group = grp,
    pattern = "conjure-log-*",
    callback = function(event)
      vim.defer_fn(function()
        vim.lsp.for_each_buffer_client(event.buf, function(_, client_id)
          vim.lsp.buf_detach_client(event.buf, client_id)
        end)
      end, 1000)
      vim.diagnostic.disable(event.buf)
    end,
  })

  local function connect_cmd()
    vim.api.nvim_feedkeys(":ConjureConnect localhost:", "n", false)
  end

  local conjure_mappings = require("config.plugins.conjure.extension").config().mappings
  local portal_mappings = require("config.plugins.conjure.portal")
  local repl = require("config.tools.nrepl-finder")

  local mappings = vim.tbl_deep_extend("force", {
    R = {
      f = { repl.find_repls, "Find REPL" },
      c = { connect_cmd, "Connect to specific port" },
      d = {"", "Disconnect from current REPL"},
      s = { repl.switch_active_repl, "Switch active REPL" },
    },
  }, conjure_mappings)
  mappings = vim.tbl_deep_extend("force", mappings, portal_mappings)
  local wk = require("which-key")
  wk.register(mappings, { prefix = "<localleader>" })
end

return M
