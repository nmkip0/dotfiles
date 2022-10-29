-- plugin to pick windows (also used by other plugins such as neo-tree, winshift)
local window_picker_ok, window_picker = pcall(require, "window-picker")
if not window_picker_ok then
  return
end

window_picker.setup(
  {
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
)

vim.api.nvim_create_user_command("PickWindowOther",
  function()
   local picked_window_id = window_picker.pick_window() or vim.api.nvim_get_current_win()
    vim.api.nvim_set_current_win(picked_window_id)
  end, {})

-- Plugin to move windows around
local winshift_ok, winshift = pcall(require, "winshift")
if not winshift_ok then
  return
end

winshift.setup({
  highlight_moving_win = true,  -- Highlight the window being moved
  focused_hl_group = "Visual",  -- The highlight group used for the moving window
  moving_win_options = {
    -- These are local options applied to the moving window while it's
    -- being moved. They are unset when you leave Win-Move mode.
    wrap = false,
    cursorline = false,
    cursorcolumn = false,
    colorcolumn = "",
  },
  keymaps = {
    disable_defaults = false, -- Disable the default keymaps
    win_move_mode = {
      ["h"] = "left",
      ["j"] = "down",
      ["k"] = "up",
      ["l"] = "right",
      ["H"] = "far_left",
      ["J"] = "far_down",
      ["K"] = "far_up",
      ["L"] = "far_right",
      ["<left>"] = "left",
      ["<down>"] = "down",
      ["<up>"] = "up",
      ["<right>"] = "right",
      ["<S-left>"] = "far_left",
      ["<S-down>"] = "far_down",
      ["<S-up>"] = "far_up",
      ["<S-right>"] = "far_right",
    },
  },
  window_picker = function ()
    return window_picker.pick_window()
  end
})

-- Plugin to maximize windows
local maximize_ok, maximize = pcall(require, "maximize")
if not maximize_ok then
  return
end

maximize.setup()

vim.api.nvim_create_user_command("ToggleMaximize",maximize.toggle , {})
