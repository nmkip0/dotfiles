local wezterm = require("wezterm")
local theme = require("theme")
local tab = require("tab")
-- local keymap = require("keymap")

local config = wezterm.config_builder()

-- config.font = wezterm.font("JetBrainsMono Nerd Font Mono")
config.font = wezterm.font("FiraCode Nerd Font Mono")
config.enable_tab_bar = true
config.scrollback_lines = 3000

-- keys
local act = wezterm.action
config.leader = { key = "a", mods = "CTRL", timeout_milliseconds = 1000 }
config.keys = {
  -- Send C-a when pressing C-a twice
  { key = "a", mods = "LEADER",       action = act.SendKey { key = "a", mods = "CTRL" } },
  { key = "c", mods = "LEADER",       action = act.ActivateCopyMode },
  
 -- Pane keybindings
  { key = "-", mods = "LEADER",       action = act.SplitVertical { domain = "CurrentPaneDomain" } },
  { key = "|", mods = "LEADER|SHIFT", action = act.SplitHorizontal { domain = "CurrentPaneDomain" } },
  { key = "h", mods = "LEADER",       action = act.ActivatePaneDirection("Left") },
  { key = "j", mods = "LEADER",       action = act.ActivatePaneDirection("Down") },
  { key = "k", mods = "LEADER",       action = act.ActivatePaneDirection("Up") },
  { key = "l", mods = "LEADER",       action = act.ActivatePaneDirection("Right") },
  { key = "x", mods = "LEADER",       action = act.CloseCurrentPane { confirm = true } },
  { key = "z", mods = "LEADER",       action = act.TogglePaneZoomState },
  { key = "s", mods = "LEADER",       action = act.RotatePanes "Clockwise" },

  { key = "r", mods = "LEADER",       action = act.ActivateKeyTable { name = "resize_pane", one_shot = false } },

  -- Tab keybindings
  { key = "n", mods = "LEADER",       action = act.SpawnTab("CurrentPaneDomain") },
  { key = "[", mods = "LEADER",       action = act.ActivateTabRelative(-1) },
  { key = "]", mods = "LEADER",       action = act.ActivateTabRelative(1) },
  { key = "t", mods = "LEADER",       action = act.ShowTabNavigator },
  -- Key table for moving tabs around
  { key = "m", mods = "LEADER",       action = act.ActivateKeyTable { name = "move_tab", one_shot = false } },

  -- Lastly, workspace
  { key = "w", mods = "LEADER",       action = act.ShowLauncherArgs { flags = "FUZZY|WORKSPACES" } },
}

for i = 1, 9 do
  table.insert(config.keys, {
    key = tostring(i),
    mods = "LEADER",
    action = act.ActivateTab(i - 1)
  })
end

config.key_tables = {
  resize_pane = {
    { key = "h",      action = act.AdjustPaneSize { "Left", 1 } },
    { key = "j",      action = act.AdjustPaneSize { "Down", 1 } },
    { key = "k",      action = act.AdjustPaneSize { "Up", 1 } },
    { key = "l",      action = act.AdjustPaneSize { "Right", 1 } },
    { key = "Escape", action = "PopKeyTable" },
    { key = "Enter",  action = "PopKeyTable" },
  },
  move_tab = {
    { key = "h",      action = act.MoveTabRelative(-1) },
    { key = "j",      action = act.MoveTabRelative(-1) },
    { key = "k",      action = act.MoveTabRelative(1) },
    { key = "l",      action = act.MoveTabRelative(1) },
    { key = "Escape", action = "PopKeyTable" },
    { key = "Enter",  action = "PopKeyTable" },
  }
}

-- window_decorations = 'RESIZE',
config.font_size = 9
config.cursor_thickness = "0.1cell"
config.window_frame = {
  border_left_width = "0cell",
  border_right_width = "0cell",
  border_bottom_height = "0cell",
  border_top_height = "0cell",
}
-- window_decorations = 'NONE',
config.window_padding = {
  left = "0cell",
  right = "0cell",
  top = "0cell",
  bottom = "0cell",
}

config.force_reverse_video_cursor = true

theme.setup(config)
tab.setup(config)

config.color_scheme = "nord"

return config
