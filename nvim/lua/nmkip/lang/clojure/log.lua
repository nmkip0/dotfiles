local M = {}

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

function M.toggle()
  local log = require("conjure.log")
  log.toggle()
  if is_log_win_open() and log["last-open-cmd"] == "split" then
    local win = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_height(win, 10)
  end
end

function M.open_vertical_split()
  conjure_log_open(true)
end

function M.open_horizontal_split()
  conjure_log_open(false)
end

return M
