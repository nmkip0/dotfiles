local win_opts = { float_opts = { border = "rounded" } }
local tb = require("telescope.builtin")

local function diag_next()
	vim.diagnostic.goto_next(win_opts)
end

local function diag_prev()
	vim.diagnostic.goto_prev(win_opts)
end

local function diag_float()
	vim.diagnostic.open_float(win_opts.float_opts)
end

local common_lsp = {
}

return {
  g = {
    d = { ":Telescope lsp_definitions<cr>", "Go to Definition" },
    i = { ":Telescope lsp_implementations<cr>", "Go to Impementations" },
    r = { ":Telescope lsp_references<cr>", "Symbol References" },
    t = { ":Telescope lsp_type_definitions<cr>", "Type Definitions" },
  },
  ["<leader>"] = {
    c = {
      name = "+code",
      a = { vim.lsp.buf.code_action, "Code actions"},
      d = {"<cmd>TroubleToggle document_diagnostics<cr>" , "Document diagnostics" },
      D = { "<cmd>TroubleToggle workspace_diagnostics<cr>" , "Workspace diagnostics" },
      j = { diag_next, "Next Diagnostic" },
      k = { diag_prev, "Prev Diagnostic" },
      l = { diag_float, "Line diagnostic" },
      o = { "<cmd>OrganizeImports<cr>", "Organize Imports" },
      r = { vim.lsp.buf.rename, "Rename"},
      s = { tb.lsp_document_symbols, "Document symbols" },
      I = { ":LspInfo<cr>", "Lsp Info" },
      R = { ":LspRestart<cr>", "Lsp Restart" },
      S = { tb.lsp_dynamic_workspace_symbols, "Workspace symbols" },
    },
    w = {
      name = "+window",
      d = { "<cmd>close<cr>", "Delete window" },
      h = { "<C-w>h", "Window left" },
      j = { "<C-w>j", "Window down" },
      k = { "<C-w>k", "Window up" },
      l = { "<C-w>l", "Window right" },
      s = { "<cmd>split<cr>", "Split horizontal" },
      v = { "<cmd>vsplit<cr>", "Split vertical" },
    }
  },
  K = { vim.lsp.buf.hover, "Hover doc" },
}
