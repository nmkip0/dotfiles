local status_ok, maximize = pcall(require, "maximize")
if not status_ok then
  return
end

maximize.setup()

vim.api.nvim_create_user_command("ToggleMaximize",maximize.toggle , {})
