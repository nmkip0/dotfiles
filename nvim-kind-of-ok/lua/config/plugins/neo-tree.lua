local M = {
	"nvim-neo-tree/neo-tree.nvim",
	cmd = "Neotree",
	branch = "v2.x",
	event = "VeryLazy",
	dependencies = {
		"kyazdani42/nvim-web-devicons",
		"s1n7ax/nvim-window-picker",
		"nvim-lua/plenary.nvim",
		"MunifTanjim/nui.nvim",
	},
}

local function get_palette()
	local theme = vim.g.colors_name

	if theme == "catppuccin" then
		local cp = require("catppuccin.palettes")
		local cp_colors = cp.get_palette("mocha")
		return {
			win_hl = cp_colors.blue,
			fg = cp_colors.crust,
		}
	end
	-- fallback
	return {
		win_hl = "#000",
		fg = "#999",
	}
end

function M.config()
	local wp = require("window-picker")
	local palette = get_palette()
	local nt = require("neo-tree")

	nt.setup({
		window = {
			mappings = {
				["<tab>"] = "toggle_node",
				["<space>"] = false,
				-- TODO: BUG if no other windows are opened
				--        ["<cr>"] = "open_with_window_picker",
				--        ["l"] = "open_with_window_picker",
				["h"] = "close_node",
				["z"] = "close_all_nodes",
				["s"] = "split_with_window_picker",
				["v"] = "vsplit_with_window_picker",
			},
		},
		filesystem = {
			cwd_target = { sidebar = "tab", current = "tab" },
			follow_current_file = true,
			group_empty_dirs = false,
			use_libuv_file_watcher = true,
			filtered_items = {
				visible = false, -- when true, they will just be displayed differently than normal items
				hide_dotfiles = true,
				hide_gitignored = true,
				hide_hidden = false, -- only works on Windows for hidden files/directories
				hide_by_name = {
					".DS_Store",
					"thumbs.db",
					"node_modules",
				},
				hide_by_pattern = {
					--"*.meta",
					--"*/src/*/tsconfig.json",
				},
				always_show = { -- remains visible even if other settings would normally hide it
					--".gitignored",
				},
				never_show = { -- remains hidden even if visible is toggled to true, this overrides always_show
					--".DS_Store",
					--"thumbs.db",
				},
				never_show_by_pattern = { -- uses glob style patterns
					--".null-ls_*",
				},
			},
		},
	})

	wp.setup({
		autoselect_one = true,
		other_win_hl_color = palette.win_hl,
		fg_color = palette.fg,
		filter_rules = {
			bo = {
				filetype = { "neo-tree", "neo-tree-popup", "notify" },
				buftype = { "terminal", "quickfix" },
			},
			file_name_contains = { "conjure-log-*" },
		},
		include_current = false,
	})
end

return M
