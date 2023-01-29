return {
    "guns/vim-sexp",

    ft = { "clojure", "lisp", "fennel", "scheme", "janet" },

    init = function()
      vim.g.sexp_filetypes = "clojure,scheme,lisp,fennel,janet"
    end,

    dependencies = {
      "radenling/vim-dispatch-neovim",
      "tpope/vim-sexp-mappings-for-regular-people",
      "tpope/vim-repeat"
    }
}
