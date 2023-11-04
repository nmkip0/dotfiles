return {
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      plugins = { spelling = true },
      defaults = {
        mode = { "n", "v" },
        ["g"] = { name = "+goto" },
        ["gs"] = { name = "+surround" },
        ["]"] = { name = "+next" },
        ["["] = { name = "+prev" },
        ["<leader><tab>"] = { name = "+tabs" },
        ["<leader>b"] = { name = "+buffer" },
        ["<leader>c"] = { name = "+code" },
        ["<leader>f"] = { name = "+file/find" },
        ["<leader>g"] = { name = "+git" },
        ["<leader>gh"] = { name = "+hunks" },
        ["<leader>q"] = { name = "+quit/session" },
        ["<leader>s"] = { name = "+search" },
        ["<leader>u"] = { name = "+ui" },
        ["<leader>w"] = { name = "+windows" },
        ["<leader>x"] = { name = "+diagnostics/quickfix" },
        ["<leader>L"] = { name = "+lazy"}
      },
    },
    config = function(_, opts)
      local wk = require("which-key")
      wk.setup(opts)
      wk.register(opts.defaults)
    end,
  },
  { "echasnovski/mini.bufremove", event = "BufReadPost" },
  {
    "folke/trouble.nvim",
    cmd = { "TroubleToggle", "Trouble" },
    keys = {
      { "<localleader>D", "<cmd>Trouble<cr>", desc = "Open Trouble Diagnostics" },
    },
    opts = {
      use_diagnostic_signs = true,
      mode = "document_diagnostics",
    },
  },
  {
    "kylechui/nvim-surround",
    event = "BufReadPost",
    opts = {
      keymaps = {
        visual = "gS",
        visual_line = "gS",
      },
    },
  },
  {
    "mbbill/undotree",
    cmd = "UndotreeToggle",
    keys = {
      {
        "<leader>u",
        function()
          vim.cmd.UndotreeToggle()
        end,
        desc = "Toggle UndoTree",
      },
    },
    init = function()
      vim.g["undotree_WindowLayout"] = 3
      vim.g["undotree_SplitWidth"] = 60
      vim.g["undotree_SetFocusWhenToggle"] = 1
    end,
  },
  -- search/replace in multiple files
  {
    "nvim-pack/nvim-spectre",
    build = false,
    cmd = "Spectre",
    opts = { open_cmd = "noswapfile vnew" },
    -- stylua: ignore
    keys = {
      { "<leader>sr", function() require("spectre").open() end, desc = "Replace in files (Spectre)" },
    },
  },
  {
    "numToStr/Comment.nvim",
    event = "BufReadPost",
    config = function()
      require("Comment").setup({})
    end,
  },
  {
    "echasnovski/mini.pairs",
    version = "0.9.0",
    config = function()
      require("mini.pairs").setup({
        mappings = {
          ["<"] = { action = "open", pair = "<>", neigh_pattern = "[^\\]." },
          [">"] = { action = "close", pair = "<>", neigh_pattern = "[^\\]." },
        },
      })
    end,
  },
  -- Fuzzy finder.
  -- The default key bindings to find files will use Telescope's
  -- `find_files` or `git_files` depending on whether the
  -- directory is a git repo.
  {
    "nvim-telescope/telescope.nvim",
    cmd = "Telescope",
    version = false, -- telescope did only one release, so use HEAD for now
    keys = {
      {
        "<leader>,",
        "<cmd>Telescope buffers sort_mru=true sort_lastused=true<cr>",
        desc = "Switch Buffer",
      },
      { "<leader>/", "<cmd>Telescope live_grep<cr>", desc = "Grep (root dir)" },
      { "<leader>:", "<cmd>Telescope command_history<cr>", desc = "Command History" },
      -- buffers
      { "<leader>bb", "<cmd>Telescope buffers sort_mru=true sort_lastused=true<cr>", desc = "Buffers" },
      -- files
      { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find files" },
      { "<leader>fr", "<cmd>Telescope oldfiles cwd_only=true<cr>", desc = "Open Recent" },
      -- git
      { "<leader>gb", "<cmd>Telescope git_branches<CR>", desc = "Branches" },
      { "<leader>gc", "<cmd>Telescope git_commits<CR>", desc = "Commits" },
      { "<leader>gs", "<cmd>Telescope git_status<CR>", desc = "Status" },
      -- search
      { '<leader>s"', "<cmd>Telescope registers<cr>", desc = "Registers" },
      { "<leader>sa", "<cmd>Telescope autocommands<cr>", desc = "Auto Commands" },
      { "<leader>sb", "<cmd>Telescope current_buffer_fuzzy_find<cr>", desc = "Buffer" },
      { "<leader>sc", "<cmd>Telescope command_history<cr>", desc = "Command History" },
      { "<leader>sC", "<cmd>Telescope commands<cr>", desc = "Commands" },
      { "<leader>sd", "<cmd>Telescope diagnostics bufnr=0<cr>", desc = "Document diagnostics" },
      { "<leader>sD", "<cmd>Telescope diagnostics<cr>", desc = "Workspace diagnostics" },
      { "<leader>sh", "<cmd>Telescope help_tags<cr>", desc = "Help Pages" },
      { "<leader>sH", "<cmd>Telescope highlights<cr>", desc = "Search Highlight Groups" },
      { "<leader>sk", "<cmd>Telescope keymaps<cr>", desc = "Key Maps" },
      { "<leader>sM", "<cmd>Telescope man_pages<cr>", desc = "Man Pages" },
      { "<leader>sm", "<cmd>Telescope marks<cr>", desc = "Jump to Mark" },
      { "<leader>so", "<cmd>Telescope vim_options<cr>", desc = "Options" },
      { "<leader><leader>", "<cmd>Telescope resume<cr>", desc = "Resume" },
      {
        "<leader>ss",
        function()
          require("telescope.builtin").lsp_document_symbols({
            symbols = require("lazyvim.config").get_kind_filter(),
          })
        end,
        desc = "Goto Symbol",
      },
      {
        "<leader>sS",
        function()
          require("telescope.builtin").lsp_dynamic_workspace_symbols({
            symbols = require("lazyvim.config").get_kind_filter(),
          })
        end,
        desc = "Goto Symbol (Workspace)",
      },
    },
    opts = function()
      local actions = require("telescope.actions")

      local open_with_trouble = function(...)
        return require("trouble.providers.telescope").open_with_trouble(...)
      end
      local open_selected_with_trouble = function(...)
        return require("trouble.providers.telescope").open_selected_with_trouble(...)
      end

      return {
        pickers = {
          find_files = {
            find_command = {
              "rg",
              "--files",
              "--hidden",
              "--ignore",
              "-u",
              "--trim",
              "--smart-case",
              "--max-columns=150",

              "--glob=!**/.git/*",
              "--glob=!**/node_modules/*",
              "--glob=!**/.next/*",
              "--glob=!**/target/*",
              "--glob=!**/.shadow-cljs/*",
              "--glob=!**/.cpcache/*",
              "--glob=!**/.cache/*",
              "--glob=!*-lock.*",
              "--glob=!*.log",
            },
          },
        },
        defaults = {
          layout_config = {
            vertical = { width = 0.9 },
          },
          layout_strategy = "vertical",
          prompt_prefix = " ",
          selection_caret = " ",
          -- open files in the first window that is an actual file.
          -- use the current window if no other window is available.
          get_selection_window = function()
            local wins = vim.api.nvim_list_wins()
            table.insert(wins, 1, vim.api.nvim_get_current_win())
            for _, win in ipairs(wins) do
              local buf = vim.api.nvim_win_get_buf(win)
              if vim.bo[buf].buftype == "" then
                return win
              end
            end
            return 0
          end,
          mappings = {
            i = {
              ["<c-t>"] = open_with_trouble,
              ["<a-t>"] = open_selected_with_trouble,
              -- check lazy editor.lua
              -- ["<a-i>"] = find_files_no_ignore,
              -- ["<a-h>"] = find_files_with_hidden,
              ["<C-Down>"] = actions.cycle_history_next,
              ["<C-Up>"] = actions.cycle_history_prev,
              ["<C-f>"] = actions.preview_scrolling_down,
              ["<C-b>"] = actions.preview_scrolling_up,
            },
            n = {
              ["q"] = actions.close,
            },
          },
        },
      }
    end,
  },
  -- A telescope.nvim extension designed to provide the best possible suggestions for quickly opening files in Neovim.
  -- smart-open will improve its suggestions over time, adapting to your usage.
  {
    "danielfalk/smart-open.nvim",
    -- branch = "0.2.x",
    branch = "main",
    config = function()
      require("telescope").load_extension("smart_open")
    end,
    keys = {
      {
        "<leader><space>",
        function()
          require("telescope").extensions.smart_open.smart_open({
            cwd_only = true,
            match_algorithm = "fzy",
            filename_first = false,
          })
        end,
        desc = "Find files",
      },
    },
    dependencies = {
      "kkharji/sqlite.lua",
      "nvim-telescope/telescope-fzy-native.nvim",
    },
  },

}
