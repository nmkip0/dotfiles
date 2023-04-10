local M = {
  "nvim-telescope/telescope.nvim",
  cmd = "Telescope",
  branch = "0.1.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope-ui-select.nvim",
    "natecraddock/workspaces.nvim",
    "tsakirist/telescope-lazy.nvim",
    {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
    },
  },
}

function M.config()
  local telescope = require("telescope")
  local actions = require("telescope.actions")
  local themes = require("telescope.themes")
  require("telescope.config")
  telescope.setup({
    defaults = {

      prompt_prefix = " ",
      selection_caret = " ",
      path_display = { "smart" },
      dynamic_preview_title = true,
      layout_config = {
        vertical = { width = 0.9 },
      },
      layout_strategy = "vertical",
      -- file_ignore_patterns = { "node_modules", "target", "classes", ".cpcache", ".cache", ".git" },
      mappings = {
        i = {
          ["<C-n>"] = actions.cycle_history_next,
          ["<C-p>"] = actions.cycle_history_prev,

          ["<C-j>"] = actions.move_selection_next,
          ["<C-k>"] = actions.move_selection_previous,

          ["<C-c>"] = actions.close,

          ["<Down>"] = actions.move_selection_next,
          ["<Up>"] = actions.move_selection_previous,

          ["<CR>"] = actions.select_default,
          ["<C-x>"] = actions.select_horizontal,
          ["<C-v>"] = actions.select_vertical,
          ["<C-t>"] = actions.select_tab,

          ["<C-u>"] = actions.preview_scrolling_up,
          ["<C-d>"] = actions.preview_scrolling_down,

          ["<PageUp>"] = actions.results_scrolling_up,
          ["<PageDown>"] = actions.results_scrolling_down,

          ["<Tab>"] = actions.toggle_selection + actions.move_selection_worse,
          ["<S-Tab>"] = actions.toggle_selection + actions.move_selection_better,
          ["<C-q>"] = actions.send_to_qflist + actions.open_qflist,
          ["<M-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
          ["<C-l>"] = actions.complete_tag,
          ["<C-_>"] = actions.which_key, -- keys from pressing <C-/>
        },
        n = {
          ["<esc>"] = actions.close,
          ["<CR>"] = actions.select_default,
          ["<C-x>"] = actions.select_horizontal,
          ["<C-v>"] = actions.select_vertical,
          ["<C-t>"] = actions.select_tab,

          ["<Tab>"] = actions.toggle_selection + actions.move_selection_worse,
          ["<S-Tab>"] = actions.toggle_selection + actions.move_selection_better,
          ["<C-q>"] = actions.send_to_qflist + actions.open_qflist,
          ["<M-q>"] = actions.send_selected_to_qflist + actions.open_qflist,

          ["j"] = actions.move_selection_next,
          ["k"] = actions.move_selection_previous,
          ["H"] = actions.move_to_top,
          ["M"] = actions.move_to_middle,
          ["L"] = actions.move_to_bottom,

          ["<Down>"] = actions.move_selection_next,
          ["<Up>"] = actions.move_selection_previous,
          ["gg"] = actions.move_to_top,
          ["G"] = actions.move_to_bottom,

          ["<C-u>"] = actions.preview_scrolling_up,
          ["<C-d>"] = actions.preview_scrolling_down,

          ["<PageUp>"] = actions.results_scrolling_up,
          ["<PageDown>"] = actions.results_scrolling_down,

          ["?"] = actions.which_key,
        },
      },
      vimgrep_arguments = {
        "rg",
        -- search all known types
        "--type=all",
        -- ignore this dirs
        "--glob=!**/.git/*",
        "--glob=!**/node_modules/*",
        "--glob=!**/.next/*",
        "--glob=!**/target/*",
        "--glob=!**/.shadow-cljs/*",
        "--glob=!**/.cpcache/*",
        "--glob=!**/.cache/*",
        "--glob=!*-lock.*",
        "--glob=!*.log",
        -- limit of sin, lines longer than 150 is not relevant
        "--max-columns=150",
        "--hidden",
        "--no-ignore-vcs",
        "--color=never",
        "--no-heading",
        "--with-filename",
        "--line-number",
        "--column",
        "--smart-case",
        "--trim",
      },
    },
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
    extensions = {
      ["ui-select"] = { themes.get_dropdown({}) },
      fzf = {
        fuzzy = true,
        override_generic_sorter = true,
        override_file_sorter = true,
        case_mode = "smart_case",
      },
      lazy = {
        -- Optional theme (the extension doesn't set a default theme)
        theme = "ivy",
        -- Whether or not to show the icon in the first column
        show_icon = true,
        -- Mappings for the actions
        mappings = {
          open_in_browser = "<C-o>",
          open_in_file_browser = "<M-b>",
          open_in_find_files = "<C-f>",
          open_in_live_grep = "<C-g>",
          open_plugins_picker = "<C-b>", -- Works only after having called first another action
          open_lazy_root_find_files = "<C-r>f",
          open_lazy_root_live_grep = "<C-r>g",
        },
      },
    },
  })
  telescope.load_extension("ui-select")
  telescope.load_extension("fzf")
  telescope.load_extension("lazy")
  telescope.load_extension("workspaces")
end

return M
