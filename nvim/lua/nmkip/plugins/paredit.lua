return {
  {
    "julienvincent/nvim-paredit",
    -- dir = "~/code/nvim-paredit",
    ft = { "clojure" },
    config = function()
      local paredit = require("nvim-paredit")
      paredit.setup({
        indent = {
          enabled = true,
        },

        keys = {
          [">)"] = false,
          [">("] = false,
          ["<)"] = false,
          ["<("] = false,
          [">e"] = false,
          ["<e"] = false,
          [">f"] = false,
          ["<f"] = false,
          ["<localleader>o"] = false,
          ["<localleader>O"] = false,

          ["<S-L>"] = { paredit.api.slurp_forwards, "Slurp" },
          ["<S-H>"] = { paredit.api.barf_forwards, "Barf" },

          ["<C-H>"] = { paredit.api.slurp_backwards, "Slurp backwards" },
          ["<C-L>"] = { paredit.api.barf_backwards, "Barf backwards" },

          ["<M-h>"] = { paredit.api.drag_element_backwards, "Drag element left" },
          ["<M-l>"] = { paredit.api.drag_element_forwards, "Drag element right" },

          ["<M-S-h>"] = { paredit.api.drag_form_backwards, "Drag form left" },
          ["<M-S-l>"] = { paredit.api.drag_form_forwards, "Drag form right" },

          ["<localleader>r"] = { paredit.api.raise_element, "Raise element" },
          ["<localleader>R"] = { paredit.api.raise_form, "Raise form" },


          ["<localleader>ws"] = {
            function()
              paredit.wrap.wrap_enclosing_form_under_cursor("(sc.api/spy ", ")")
            end,
            "Wrap with sc.api/spy",
          },

          ["<localleader>wt"] = {
            function()
              paredit.wrap.wrap_enclosing_form_under_cursor("(doto ", " tap>)")
            end,
            "Wrap with doto tap>",
          },
        },
      })
    end,
  },
  {
    "julienvincent/nvim-paredit-fennel",
    -- dir = "~/code/nvim-paredit-fennel",
    dependencies = { "julienvincent/nvim-paredit" },
    ft = { "fennel" },
    config = function()
      require("nvim-paredit-fennel").setup()
    end,
  },
}
