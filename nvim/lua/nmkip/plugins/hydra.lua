return {
  {
    "anuvyklack/hydra.nvim",
    event = "VeryLazy",
    config = function()
      -- require("nmkip.hydras.window").create()
      require("nmkip.hydras.portal").create()
    end,
  },
}
