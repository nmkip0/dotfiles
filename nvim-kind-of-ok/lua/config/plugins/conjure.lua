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
  vim.g["conjure#mapping#eval_root_form"] = false
  vim.g["conjure#mapping#eval_word"] = false
  vim.g["conjure#mapping#eval_motion"] = false
  vim.g["conjure#mapping#eval_file"] = false

  -- vim.g["conjure#mapping#eval_comment_current_form"] = true
  -- vim.g["conjure#mapping#eval_comment_root_form"] = true
  -- vim.g["conjure#mapping#eval_buf"] = true
  -- vim.g["conjure#mapping#eval_visual"] = true
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

  local function conjure_log_open(is_vertical)
    local log = require("conjure.log")
    log["close-visible"]()
    local cur_log
    if is_vertical then
      log.vsplit()
      cur_log = "vsplit"
    else
      log.split()
      local win = vim.api.nvim_get_current_win()
      vim.api.nvim_win_set_height(win, 10)
      cur_log = "split"
    end
    log["last-open-cmd"] = cur_log
  end

  local function is_log_win_open()
    local l = require("conjure.log")
    local wins = l["aniseed/locals"]["find-windows"]()
    for _, _ in pairs(wins) do
      return true
    end
    return false
  end

  local function conjure_log_toggle()
    local log = require("conjure.log")
    log.toggle()
    if is_log_win_open() and log["last-open-cmd"] == "split" then
      local win = vim.api.nvim_get_current_win()
      vim.api.nvim_win_set_height(win, 10)
    end
  end

  local conjure_mappings = require("config.plugins.conjure.extension").config().mappings
  local portal_mappings = require("config.plugins.conjure.portal")
  local repl = require("config.tools.nrepl-finder")

  local mappings = vim.tbl_deep_extend("force", {
    c = {
      f = { repl.find_repls, "Find REPL" },
      C = { connect_cmd, "Connect to specific port" },
    },
    l = {
      name = "Conjure Log",
      g = {
        function()
          conjure_log_toggle()
        end,
        "Toggle",
      },
      v = {
        function()
          conjure_log_open(true)
        end,
        "Open VSplit",
      },
      s = {
        function()
          conjure_log_open(false)
        end,
        "Open Split",
      },
    },
  }, conjure_mappings)
  mappings = vim.tbl_deep_extend("force", mappings, portal_mappings)
  local wk = require("which-key")
  wk.register(mappings, { prefix = "<localleader>" })
end

return M
