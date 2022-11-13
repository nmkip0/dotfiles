-- Disable tmux navigator when zooming the Vim pane
vim.g["tmux_navigator_disable_when_zoomed"] = 1

--If the tmux window is zoomed, keep it zoomed when moving from Vim to another pane
-- Has no effect if `tmux_navigator_disable_when_zoomed` is enabled
vim.g["tmux_navigator_preserve_zoom"] = 1
