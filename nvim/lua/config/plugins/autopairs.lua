return {
	"windwp/nvim-autopairs",
	event = "VeryLazy",
	config = function()
		-- remove add single quote on filetype scheme or lisp
		-- require("nvim-autopairs").get_rule("'")[1].not_filetypes = { "clojure", "scheme", "lisp" }

		-- local cmp_autopairs = require "nvim-autopairs.completion.cmp"
		-- cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done { map_char = { tex = "" } })

		-- TODO: delete pairs with BS
		-- TODO: (aa bb cc) | deleting with BS here should delete cc bb aa and then the pair
		return {
			check_ts = true,
			ts_config = {
				lua = { "string", "source" },
				javascript = { "string", "template_string" },
				java = false,
			},
			enable_check_bracket_line = false,
			disable_filetype = { "TelescopePrompt", "spectre_panel" },
		}
	end,
}
