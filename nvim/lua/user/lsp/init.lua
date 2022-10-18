local status_ok, lsp = pcall(require, "lspconfig")
if not status_ok then
    return
end

require("user.lsp.lsp-installer")
local lsp_config = require("user.lsp.handlers")

lsp_config.setup()

lsp.clojure_lsp.setup {
    on_attach = lsp_config.on_attach,
    flags = {debounce_text_changes = 150},
    capabilities = lsp_config.capabilities,
    -- settings = {}
}
