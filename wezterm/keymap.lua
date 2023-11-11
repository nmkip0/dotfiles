local wezterm = require 'wezterm'
local act = wezterm.action
local config = {}


config.keys = {}
for i = 1, 8 do
  -- F1 through F8 to activate that tab
  table.insert(config.keys, {
    key = 'F' .. tostring(i),
    action = act.ActivateTab(i - 1),
  })
end

return config 
