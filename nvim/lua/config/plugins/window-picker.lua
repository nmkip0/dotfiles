local M = {
  'nvim-neo-tree/neo-tree.nvim',
  cmd          = 'Neotree',
  branch       = 'v2.x',
  dependencies = {
    's1n7ax/nvim-window-picker'
  }
}

function M.config()
  local wp = require('window-picker')

  wp.setup {
    autoselect_one = true,
    include_current = false,
    filter_rules = {
      -- filter using buffer options
      bo = {
        -- if the file type is one of following, the window will be ignored
        filetype = {'NvimTree', 'neo-tree', "neo-tree-popup", "notify", "quickfix" },

        -- if the buffer type is one of following, the window will be ignored
        buftype = { 'terminal' },
      },
    },
    other_win_hl_color = '#e35e4f',
  }
end

vim.api.nvim_create_user_command("PickWindowOther",
  function()
   local picked_window_id = require('window-picker').pick_window() or vim.api.nvim_get_current_win()
    vim.api.nvim_set_current_win(picked_window_id)
  end, {})

return M
