return {
  { "nvim-tree/nvim-web-devicons", lazy = true },
  { "MunifTanjim/nui.nvim", lazy = true },
  {
    "stevearc/dressing.nvim",
    event = "VeryLazy",
    config = function()
      require("dressing").setup({
        input = {
          insert_only = false,
          start_in_insert = true,

          win_options = {
            -- Window transparency (0-100)
            winblend = 0,
          },

          get_config = function(opts)
            if opts.kind == "center" then
              return {
                relative = "editor",
              }
            end
          end,
        },
      })
    end,
  },
  -- smooth scrolling
  {
    "psliwka/vim-smoothie",
    event = "BufReadPost",
    init = function()
      vim.g["smoothie_remapped_commands"] = { "<C-D>", "<C-U>" }
    end,
  },
  {
    "petertriho/nvim-scrollbar",
    event = "BufReadPost",
    config = function()
      require("scrollbar").setup({
        handle = {
          blend = 0,
        },
        marks = {
          Cursor = {
            text = "",
          },
        },
        handlers = {
          gitsigns = true,
        },
        excluded_filetypes = {
          "cmp_docs",
          "cmp_menu",
          "noice",
          "prompt",
          "TelescopePrompt",
          "neo-tree",
          "neo-tree-popup",
          "DiffviewFiles",
        },
      })
    end,
  },
  {
    "akinsho/bufferline.nvim",
    event = "VeryLazy",
    version = "*",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
      options = {
        mode = "tabs",
        themable = true,
        separator_style = "thin",
        always_show_bufferline = false,
      },
    },
  },
  {
    "nvim-lualine/lualine.nvim",

    event = "VeryLazy",

    config = function()
      local lualine = require("lualine")

      local function get_lsp_client_status()
        for _, client in pairs(vim.lsp.get_active_clients({ bufnr = 0 })) do
          if client.initialized then
            return ""
          else
            return ""
          end
        end
        return ""
      end

      local function get_nrepl_status()
        if vim.bo.filetype == "clojure" then
          local nrepl = require("nmkip.lang.clojure.nrepl")
          return nrepl.get_repl_status("Not Connected")
        end
        return ""
      end

      lualine.setup({
          options = {
            icons_enabled = true,
            globalstatus = true,
            theme = "auto",
          },
          sections = {
            lualine_a = {
              "mode",
            },
            lualine_b = {
              "branch",
              "diff",
              "diagnostics",
            },
            lualine_c = {
              {
                path = 1,
                "filename",

                -- Small attempt to workaround https://github.com/nvim-lualine/lualine.nvim/issues/872
                -- Upstream issue: https://github.com/neovim/neovim/issues/19464
                fmt = function(filename)
                  if #filename > 80 then
                    filename = vim.fs.basename(filename)
                  end
                  return filename
                end,
              },
            },

            lualine_x = {
              {
                "lsp_client_status",
                fmt = get_lsp_client_status,
                color = {
                  fg = "#b0b846",
                },
              },
            },
            lualine_y = {
              {
                fmt = get_nrepl_status,
                "repl_status",
              },
            },
            lualine_z = {
              "filetype",
            },
          },
      })
    end,
  },
}
