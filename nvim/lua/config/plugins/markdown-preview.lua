return {
    "iamcco/markdown-preview.nvim",
    ft = "markdown",
    build = "cd app && pnpm i",
    init = function()
      vim.g.mkdp_filetypes = { "markdown" }
    end
}
