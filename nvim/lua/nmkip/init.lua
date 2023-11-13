require("nmkip.core.lazy")
require("nmkip.core.keymaps")
require("nmkip.core.options")
require("nmkip.core.autocmds")

require("nmkip.lang.authzed").setup()

require("nmkip.behaviours.auto-save").setup()
require("nmkip.behaviours.inter-process-yank").setup()
require("nmkip.behaviours.yank-to-clipboard").setup()
