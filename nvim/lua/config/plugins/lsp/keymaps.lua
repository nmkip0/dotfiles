local win_opts = { float_opts = { border = 'rounded' } }
local tb = require('telescope.builtin')

local function diag_next()
  vim.diagnostic.goto_next(win_opts)
end

local function diag_prev()
  vim.diagnostic.goto_prev(win_opts)
end

local function diag_float()
  vim.diagnostic.open_float(win_opts.float_opts)
end

local M = {}

function M.setup(bufnr)
  return {
    g = {
      d = { tb.lsp_definitions, 'Go to definition' },
      r = { tb.lsp_references, 'LSP rerefences' },
      t = { tb.lsp_type_definitions, 'Type definition' }
    },
    ['<leader>'] = {
      c = {
        name = "+code",
        a = { vim.lsp.buf.code_action, "Code Actions" },
        d = { function() tb.diagnostics { bufnr = 0 } end, 'Document diagnostics' },
        D = { tb.diagnostics, 'Workspace diagnostics' },
        f = { vim.lsp.buf.format, 'Format buffer' },
        j = { diag_next, "Next Diagnostic", },
        k = { diag_prev, "Prev Diagnostic", },
        l = { diag_float, 'Line diagnostic' },
        o = { "<cmd>OrganizeImports<cr>", "Organize Imports" },
        r = { vim.lsp.buf.rename, 'Rename' },
        R = { ":LspRestart<cr>", 'LSP Restart' },
        s = { tb.lsp_document_symbols, 'Document symbols' },
        S = { tb.lsp_dynamic_workspace_symbols, 'Workspace symbols' },
        w = {
          name = 'LSP Workspace',
          a = { vim.lsp.buf.add_workspace_folder, 'Workspace Add Folder' },
          r = { vim.lsp.buf.remove_workspace_folder, 'Workspace Remove Folder' }
        }
      }
    },
    K = { vim.lsp.buf.hover, 'Hover doc' },
    -- ['<C-k>'] = { vim.lsp.buf.signature_help, 'Signature Documentation' }
  }
end

return M
