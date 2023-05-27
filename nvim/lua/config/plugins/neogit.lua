return {
  cmd = "Neogit",
  "CKolkey/neogit",
  opts = {
    disable_commit_confirmation = true,
    use_magit_keybindings = true,
    kind = "tab",
    commit_popup = {
      kind = "vsplit",
    },
    signs = {
      section = { "", "" },
      item = { "", "" },
    },
    mappings = {
      status = {
        ["B"] = "BranchPopup",
        ["l"] = "LogPopup",
      },
      -- Modify fuzzy-finder buffer mappings
      finder = {},
    },
  },
}
