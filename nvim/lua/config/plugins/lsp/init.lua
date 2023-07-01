local servers = {
  clojure_lsp = {
    commands = {
      OrganizeImports = {
        function()
          vim.lsp.buf.execute_command({
            command = "clean-ns",
            arguments = { "file://" .. vim.api.nvim_buf_get_name(0), 0, 0 },
            title = "",
          })
        end,
        description = "Clean Namespace",
      },
    },
    root_dir = function()
      return vim.fn.getcwd()
    end,
    init_options = {
      signatureHelp = true,
      codeLens = true,
    },
  },
  yamlls = {
    settings = {
      yaml = {
        schemas = {
          "https://json.schemastore.org/github-workflow.json",
          ".github/workflows/*",
        },
      },
    },
  },
  cssls = {},
  dockerls = {},
  svelte = {},
  eslint = {},
  html = {},
  marksman = {},
  tsserver = {},
  jdtls = {},
  jsonls = {},
  vimls = {},
  tailwindcss = {
    filetypes = { "html", "css" },
  },
  sumneko_lua = {
    single_file_support = true,
    settings = {
      Lua = {
        workspace = {
          checkThirdParty = false,
        },
        completion = {
          workspaceWord = true,
          callSnippet = "Both",
        },
        misc = {
          parameters = {
            "--log-level=trace",
          },
        },
        diagnostics = {
          -- enable = false,
          groupSeverity = {
            strong = "Warning",
            strict = "Warning",
          },
          groupFileStatus = {
            ["ambiguity"] = "Opened",
            ["await"] = "Opened",
            ["codestyle"] = "None",
            ["duplicate"] = "Opened",
            ["global"] = "Opened",
            ["luadoc"] = "Opened",
            ["redefined"] = "Opened",
            ["strict"] = "Opened",
            ["strong"] = "Opened",
            ["type-check"] = "Opened",
            ["unbalanced"] = "Opened",
            ["unused"] = "Opened",
          },
          unusedLocalExclude = { "_*" },
        },
        format = {
          enable = false,
          defaultConfig = {
            indent_style = "space",
            indent_size = "2",
            continuation_indent_size = "2",
          },
        },
      },
    },
  },
}

local M = {
  "neovim/nvim-lspconfig",
  event = "BufReadPre",
  dependencies = {
    "jose-elias-alvarez/null-ls.nvim",
    {
      "williamboman/mason.nvim",
      config = {
        ui = { border = "rounded" },
      },
    },
    {
      "folke/neodev.nvim",
      config = true,
    },
    "onsails/lspkind.nvim",
    "hrsh7th/cmp-nvim-lsp",
    "nvim-telescope/telescope.nvim",
    {
      "williamboman/mason-lspconfig.nvim",
      config = {
        ensure_installed = vim.tbl_keys(servers),
      },
    },
  },
}

function M.config()
  require("lspconfig.ui.windows").default_options.border = "rounded"
  local nls = require("null-ls")
  local mason_lspconfig = require("mason-lspconfig")
  local settings = require("config.plugins.lsp.settings")
  --require "config.plugins.lsp.diagnostics".setup()

  local options = {
    before_init = function(params)
      params.workDoneToken = "1"
    end,
    handlers = settings.handlers,
    on_attach = settings.on_attach,
    capabilities = settings.capabilities,
    flags = {
      debounce_text_changes = 150,
    },
  }

  nls.setup({
    on_attach = settings.on_attach,
    sources = {
      nls.builtins.formatting.prettierd,
      nls.builtins.formatting.stylua,
      nls.builtins.diagnostics.flake8,
      nls.builtins.formatting.just,
    },
  })

  mason_lspconfig.setup_handlers({
    function(server_name)
      local server_opts = servers[server_name] or {}
      local opts = vim.tbl_deep_extend("force", {}, options, server_opts)
      require("lspconfig")[server_name].setup(opts)
    end,
  })
end

return M
