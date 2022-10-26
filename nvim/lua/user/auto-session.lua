local status_ok, auto_session = pcall(require, "auto-session")
if not status_ok then
  return
end

vim.o.sessionoptions="blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal"

auto_session.setup {
  log_level = "error",

  cwd_change_handling = {
    restore_upcoming_session = true,
    pre_cwd_changed_hook = nil,
    post_cwd_changed_hook = nil
  },
  auto_session_suppress_dirs = { "~/", "~/Downloads", "/"},
}
