local map = require("nmkip.helpers.keys").map
local saving = require("nmkip.behaviours.auto-save")

map("n", "<leader>qq", function()
  saving.write_all_buffers()
  vim.api.nvim_command("qa")
end, "Save and quit")

map("n", "<leader>fs", "<Cmd>w<Cr>", "Save buffer")
map("n", "<leader>fS", saving.write_all_buffers, "Save all buffers")

map({ "i", "n" }, "<esc>", "<cmd>noh<cr><esc>", "Escape and clear hlsearch")

map("n", "<leader>`", "<C-^>", "Toggle previous buffer")

---- Move to window using <ctrl>-hjkl
map("n", "<C-h>", "<C-w>h", "Go to left window")
map("n", "<C-j>", "<C-w>j", "Go to lower window")
map("n", "<C-k>", "<C-w>k", "Go to upper window")
map("n", "<C-l>", "<C-w>l", "Go to right window")

-- tabs
map("n", "<leader><tab>l", "<cmd>tabnext<cr>", "Next Tab")
map("n", "<leader><tab>d", "<cmd>tabclose<cr>", "Close Tab")
map("n", "<leader><tab>h", "<cmd>tabprevious<cr>", "Previous Tab")

vim.api.nvim_set_keymap("n", "<S-Down>", "<Nop>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<S-Up>", "<Nop>", { noremap = true, silent = true })
