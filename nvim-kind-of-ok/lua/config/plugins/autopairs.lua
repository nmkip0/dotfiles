return {
	"windwp/nvim-autopairs",
	event = "VeryLazy",
	opts = {
		check_ts = true,
		ts_config = {
			lua = { "string", "source" },
			javascript = { "string", "template_string" },
			java = false,
		},
		enable_check_bracket_line = false,
		disable_filetype = { "TelescopePrompt", "spectre_panel", "clojure" },
	},
}
