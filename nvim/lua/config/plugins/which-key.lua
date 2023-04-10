local M = {
	"folke/which-key.nvim",
	event = "VeryLazy",
}

local Util = require("lazy.core.util")
local function toggle(option, silent, values)
	if values then
		if vim.opt_local[option]:get() == values[1] then
			vim.opt_local[option] = values[2]
		else
			vim.opt_local[option] = values[1]
		end
		return Util.info("Set " .. option .. " to " .. vim.opt_local[option]:get(), { title = "Option" })
	end
	vim.opt_local[option] = not vim.opt_local[option]:get()
	if not silent then
		if vim.opt_local[option]:get() then
			Util.info("Enabled " .. option, { title = "Option" })
		else
			Util.warn("Disabled " .. option, { title = "Option" })
		end
	end
end

local function toggle_fn(value)
	return function()
		toggle(value)
	end
end

local leader_opts = {
	mode = "n", -- NORMAL mode
	prefix = "<leader>",
	buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
	silent = true, -- use `silent` when creating keymaps
	noremap = true, -- use `noremap` when creating keymaps
	nowait = true, -- use `nowait` when creating keymaps
}

function M.config()
	local which_key = require("which-key")
	which_key.setup({
		plugins = {
			marks = true, -- shows a list of your marks on ' and `
			registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
			spelling = {
				enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
				suggestions = 20, -- how many suggestions should be shown in the list?
			},
			-- the presets plugin, adds help for a bunch of default keybindings in Neovim
			-- No actual key bindings are created
			presets = {
				operators = true, -- adds help for operators like d, y, ... and registers them for motion / text object completion
				motions = true, -- adds help for motions
				text_objects = true, -- help for text objects triggered after entering an operator
				windows = true, -- default bindings on <c-w>
				nav = true, -- misc bindings to work with windows
				z = true, -- bindings for folds, spelling and others prefixed with z
				g = true, -- bindings for prefixed with g
			},
		},
		-- add operators that will trigger motion and text object completion
		-- to enable all native operators, set the preset / operators plugin above
		operators = { gc = "Comments" },
		key_labels = {
			-- override the label used to display some keys. It doesn't effect WK in any other way.
			-- For example:
			-- ["<space>"] = "SPC",
			-- ["<cr>"] = "RET",
			-- ["<tab>"] = "TAB",
		},
		icons = {
			breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
			separator = "➜", -- symbol used between a key and it's label
			group = "+", -- symbol prepended to a group
		},
		popup_mappings = {
			scroll_down = "<c-d>", -- binding to scroll down inside the popup
			scroll_up = "<c-u>", -- binding to scroll up inside the popup
		},
		window = {
			border = "single", -- none, single, double, shadow
			position = "bottom", -- bottom, top
			margin = { 0, 0, 0, 0 }, -- extra window margin [top, right, bottom, left]
			padding = { 0, 1, 0, 1 }, -- extra window padding [top, right, bottom, left]
			winblend = 0,
		},
		layout = {
			height = { min = 4, max = 25 }, -- min and max height of the columns
			width = { min = 20, max = 50 }, -- min and max width of the columns
			spacing = 3, -- spacing between columns
			align = "left", -- align columns left, center or right
		},
		ignore_missing = false, -- enable this to hide mappings for which you didn't specify a label
		hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " }, -- hide mapping boilerplate
		show_help = true, -- show help message on the command line when the popup is visible
		triggers = "auto", -- automatically setup triggers
		-- triggers = {"<leader>"} -- or specify a list manually
		triggers_blacklist = {
			-- list of mode / prefixes that should never be hooked by WhichKey
			-- this is mostly relevant for key maps that start with a native binding
			-- most people should not need to change this
			i = { "j", "k" },
			v = { "j", "k" },
		},
		-- disable the WhichKey popup for certain buf types and file types.
		-- Disabled by deafult for Telescope
		disable = {
			buftypes = {},
			filetypes = { "TelescopePrompt" },
		},
	})

	local mappings = {
		["`"] = { "<C-^>", "Switch to last buffer" },
		["/"] = { "<cmd>Telescope live_grep theme=ivy<cr>", "Search dir" },
		b = {
			name = "+buffer",
			b = { "<cmd>Telescope buffers theme=ivy<cr>", "Buffers" },
			d = { "<cmd>Bdelete<cr>", "Delete buffer" },
			D = { "<cmd>bufdo Bdelete<cr>", "Delete all buffer" },
			k = { "<cmd>Bwipeout<cr>", "Kill buffer (remove from jmp list)" },
		},
		f = {
			name = "+file",
			f = {
        "<cmd>Telescope find_files<cr>",
				"Find File",
			},
			r = { "<cmd>Telescope oldfiles cwd_only=true<cr>", "Open Recent File" },
			s = { "<cmd>w!<CR>", "Save" },
			S = { "<cmd>wa<cr>", "Save all" },
		},
		g = {
			name = "+git",
			j = { "<cmd>lua require 'gitsigns'.next_hunk()<cr>", "Next Hunk" },
			k = { "<cmd>lua require 'gitsigns'.prev_hunk()<cr>", "Prev Hunk" },
			l = { "<cmd>lua require 'gitsigns'.blame_line()<cr>", "Blame" },
			p = { "<cmd>lua require 'gitsigns'.preview_hunk()<cr>", "Preview Hunk" },
			r = { "<cmd>lua require 'gitsigns'.reset_hunk()<cr>", "Reset Hunk" },
			R = { "<cmd>lua require 'gitsigns'.reset_buffer()<cr>", "Reset Buffer" },
			s = { "<cmd>lua require 'gitsigns'.stage_hunk()<cr>", "Stage Hunk" },
			u = { "<cmd>lua require 'gitsigns'.undo_stage_hunk()<cr>", "Undo Stage Hunk" },
			o = { "<cmd>Telescope git_status<cr>", "Open changed file" },
			b = { "<cmd>Telescope git_branches<cr>", "Checkout branch" },
			c = { "<cmd>Telescope git_commits<cr>", "Checkout commit" },
			d = { "<cmd>Gitsigns diffthis HEAD<cr>", "Diff" },
		},
		h = {
			name = "+help",
			k = { "<cmd>Telescope keymaps<cr>", "Keymaps" },
			c = { "<cmd>Telescope commands<cr>", "Commands" },
			e = { "<cmd>:help events<cr>", "Events" },
			h = { "<cmd>Telescope help_tags<cr>", "Find Help" },
			M = { "<cmd>Telescope man_pages<cr>", "Man Pages" },
		},
		o = {
			name = "+open",
			b = { "<cmd>Neotree buffers toggle<cr>", "Sidebar" },
			p = { "<cmd>NeoTreeRevealToggle<cr>", "Sidebar" },
			P = { "<cmd>NeoTreeReveal<cr>", "Focus file in sidebar" },
			r = { "<cmd>Telescope oldfiles cwd_only=true<cr>", "Open Recent File" },
		},
		s = {
			name = "+search",
			b = { "<cmd>Telescope current_buffer_fuzzy_find<cr>", "Current buffer" },
			B = { "<cmd>Telescope git_branches<cr>", "Git branches" },
			d = { "<cmd>Telescope live_grep<cr>", "Directory" },
			l = { "<cmd>Telescope resume<cr>", "Last search" },
			k = { "<cmd>Telescope keymaps<cr>", "Keymaps" },
			r = { "<cmd>Telescope oldfiles cwd_only=true<cr>", "Open Recent File" },
			R = { "<cmd>Telescope registers<cr>", "Registers" },
			p = { "<cmd>Telescope live_grep theme=ivy<cr>", "Search project" },
			s = { "<cmd>Telescope resume<cr>", "Last search" },
			T = { "<cmd>Telescope colorscheme<cr>", "Themes" },
		},
		t = {
			name = "+toggles",
			c = { "<cmd>TSContextToggle<cr>", "Treesitter Context" },
			r = { toggle_fn("relativenumber"), "Relative Number" },
			w = { toggle_fn("wrap"), "Wrap" },
		},
		w = {
			name = "+window",
			["."] = { "<cmd>WinShift<cr>", "Win-Move mode" },
			d = { "<cmd>close<cr>", "Delete window" },
			h = { "<C-w>h", "Window left" },
			j = { "<C-w>j", "Window down" },
			k = { "<C-w>k", "Window up" },
			l = { "<C-w>l", "Window right" },
			m = { "<cmd>ToggleMaximize<cr>", "Maximize" },
			p = { "<cmd>PickWindowOther<cr>", "Pick other window" },
			s = { "<cmd>split<cr>", "Split horizontal" },
			v = { "<cmd>vsplit<cr>", "Split vertical" },
			H = { "<cmd>WinShift left<cr>", "Move window left" },
			J = { "<cmd>WinShift down<cr>", "Move window down" },
			K = { "<cmd>WinShift up<cr>", "Move window up" },
			L = { "<cmd>WinShift right<cr>", "Move window right" },
			S = { "<cmd>WinShift swap<cr>", "Swap two windows" },
			["="] = { "<C-w>=", "Balance" },
			["|"] = { "<C-w>|", "Max out width" },
			["_"] = { "<C-w>_", "Max out height" },
		},
		q = {
			name = "+quit/session",
			q = { "<cmd>qa<cr>", "Quit" },
			Q = { "<cmd>qa!<cr>", "Quit without saving" },
		},
		L = {
			name = "+lazy",
			l = { "<cmd>Lazy<cr>", "Lazy" },
			t = { "<cmd>Telescope lazy<cr>", "Telescope" },
		},
		M = {
			name = "+mason",
			m = { "<cmd>Mason<cr>", "Mason" },
		},
	}

	which_key.register(mappings, leader_opts)
end

local lisp_mappings = {
	c = { name = "+connect" },
	e = {
		name = "+eval",
		c = {
			name = "+comment",
		},
	},
	g = { name = "+get" },
	l = { name = "+conjure log" },
	r = { name = "+refresh" },
	s = {
		name = "+session",
		-- remap conjure stuff to `S` +session
		-- `s` structural editing
		b = { "<Plug>(sexp_emit_tail_element)", "Barf forward" },
		r = { "<Plug>(sexp_raise_list)", "Raise list" },
		R = { "<Plug>(sexp_raise_element)", "Raise element" },
		s = { "<Plug>(sexp_capture_next_element)", "Slurp forward" },
	},
	t = { name = "+test" },
	v = { name = "+view" },
}

local function llopts(bufnr)
	return {
		mode = "n",
		prefix = "<localleader>",
		buffer = bufnr,
		silent = true,
		noremap = true,
	}
end

-- local group_id = vim.api.nvim_create_augroup("LISP_MAPPINGS", {clear = true})
-- vim.api.nvim_create_autocmd("FileType", {
--                               pattern = { "clojure", "fennel"},
--                               group = group_id,
--                               callback = function(ctx)
--                                 local which_key = require('which-key')
--                                 which_key.register({},llopts(0))
--                                 which_key.register(lisp_mappings, llopts(ctx.buf))
--                               end
-- })

-- local conjure_portal_ok, portal_mappings = pcall(require, "user.conjure-portal")
-- if not conjure_portal_ok then
--   return
-- end

-- local portal_group_id = vim.api.nvim_create_augroup("PORTAL_MAPPINGS", {clear = true})
-- vim.api.nvim_create_autocmd("FileType", {
--                               pattern = { "clojure" },
--                               group = portal_group_id,
--                               callback = function(ctx)

--                                 local which_key = require('which-key')
--                                 which_key.register(portal_mappings, llopts(ctx.buf))
--                               end
-- })

return M
