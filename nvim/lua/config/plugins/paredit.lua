return {
  "julienvincent/nvim-paredit",
  -- dir = "~/Developer/neovim/nvim-paredit",
  ft = { "clojure" },
  enabled = true,
  config = function()
    local paredit = require("nvim-paredit")
    paredit.setup({
      filetypes = { "clojure" },
      cursor_behaviour = "auto",
      use_default_keys = false,
      keys = {
        [">)"] = { paredit.api.slurp_forwards, "Slurp forwards" },
        ["<)"] = { paredit.api.slurp_backwards, "Slurp backwards" },

        [">("] = { paredit.api.barf_forwards, "Barf forwards" },
        ["<("] = { paredit.api.barf_backwards, "Barf backwards" },

        ["<M-l>"] = { paredit.api.drag_element_forwards, "Drag element fowrad" },
        ["<M-h>"] = {
          paredit.api.drag_element_backwards,
          "Drag element backward",
        },

        [">f"] = { paredit.api.drag_form_forwards, "Drag form forward" },
        ["<f"] = { paredit.api.drag_form_backwards, "Drag form backward" },

        ["<localleader>sR"] = { paredit.api.raise_form, "Raise form" },
        ["<localleader>sr"] = { paredit.api.raise_element, "Raise element" },

        ["E"] = {
          paredit.api.move_to_next_element,
          "Jump to next element tail",
          repeatable = false,
          operator = true,
        },
        ["B"] = {
          paredit.api.move_to_prev_element,
          "Jump to previous element head",
          repeatable = false,
          operator = true,
        },
      },
    })
  end,
}
