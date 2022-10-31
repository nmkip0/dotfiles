vim.g["conjure#extract#tree_sitter#enabled"] = true
vim.g["conjure#client#clojure#nrepl#connection#auto_repl#enabled"] = false
vim.g["conjure#highlight#enabled"] = true
vim.g["conjure#highlight#timeout"] = 150
vim.g["conjure#log#wrap"] = true
vim.g["conjure#log#jump_to_latest#enabled"] = true
vim.g["conjure#log#hud#enabled"] = false
vim.g["conjure#client#clojure#nrepl#eval#raw_out"] = true
vim.g["conjure#client#clojure#nrepl#test#raw_out"] = true
vim.g["conjure#client#clojure#nrepl#test#runner"] = "kaocha"
vim.g["conjure#log#jump_to_latest#cursor_scroll_position"] = "center"

vim.api.nvim_create_autocmd("BufNewFile", {
  group = vim.api.nvim_create_augroup("CONJURE_LOG_DISABLE_LSP", {clear = true}),
  pattern = "conjure-log-*",
  callback = function()
    vim.diagnostic.disable(0)
  end
})
