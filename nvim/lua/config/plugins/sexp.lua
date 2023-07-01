local function vim_sexp_mappings()
  local function map(mode, rhs, lhs, opts)
    opts.buffer = 0
    vim.keymap.set(mode, rhs, lhs, opts)
  end

  local function xmap(rhs, lhs, opts)
    map("x", rhs, lhs, opts)
  end

  local function nmap(rhs, lhs, opts)
    map("n", rhs, lhs, opts)
  end

  local function omap(rhs, lhs, opts)
    map("o", rhs, lhs, opts)
  end

  local function imap(rhs, lhs, opts)
    map("i", rhs, lhs, opts)
  end

  xmap("af", "<Plug>(sexp_outer_list)", { desc = "Sexp outer list" })
  omap("af", "<Plug>(sexp_outer_list)", { desc = "Sexp outer list" })
  xmap("if", "<Plug>(sexp_inner_list)", { desc = "Sexp inner list" })
  omap("if", "<Plug>(sexp_inner_list)", { desc = "Sexp inner list" })
  xmap("aF", "<Plug>(sexp_outer_top_list)", { desc = "Sexp outer top list" })
  omap("aF", "<Plug>(sexp_outer_top_list)", { desc = "Sexp outer top list" })
  xmap("iF", "<Plug>(sexp_inner_top_list)", { desc = "Sexp inner top list" })
  omap("iF", "<Plug>(sexp_inner_top_list)", { desc = "Sexp inner top list" })
  xmap("as", "<Plug>(sexp_outer_string)", { desc = "Sexp outer string" })
  omap("as", "<Plug>(sexp_outer_string)", { desc = "Sexp outer string" })
  xmap("is", "<Plug>(sexp_inner_string)", { desc = "Sexp inner string" })
  omap("is", "<Plug>(sexp_inner_string)", { desc = "Sexp inner string" })
  xmap("ae", "<Plug>(sexp_outer_element)", { desc = "Sexp outer element" })
  omap("ae", "<Plug>(sexp_outer_element)", { desc = "Sexp outer element" })
  xmap("ie", "<Plug>(sexp_inner_element)", { desc = "Sexp inner element" })
  omap("ie", "<Plug>(sexp_inner_element)", { desc = "Sexp inner element" })
  nmap("(", "<Plug>(sexp_move_to_prev_bracket)", { desc = "Sexp move to prev bracket" })
  nmap(")", "<Plug>(sexp_move_to_next_bracket)", { desc = "Sexp move to next bracket" })

  nmap("B", "<Plug>(sexp_move_to_prev_element_head)", { desc = "Sexp move to prev element head" })
  xmap("B", "<Plug>(sexp_move_to_prev_element_head)", { desc = "Sexp move to prev element head" })
  omap("B", "<Plug>(sexp_move_to_prev_element_head)", { desc = "Sexp move to prev element head" })
  nmap("E", "<Plug>(sexp_move_to_next_element_tail)", { desc = "Sexp move to next element tail" })
  xmap("E", "<Plug>(sexp_move_to_next_element_tail)", { desc = "Sexp move to next element tail" })
  omap("E", "<Plug>(sexp_move_to_next_element_tail)", { desc = "Sexp move to next element tail" })

  nmap("<M-w>", "<Plug>(sexp_move_to_next_element_head)", { desc = "Sexp move to next element head" })
  xmap("<M-w>", "<Plug>(sexp_move_to_next_element_head)", { desc = "Sexp move to next element head" })
  omap("<M-w>", "<Plug>(sexp_move_to_next_element_head)", { desc = "Sexp move to next element head" })
  nmap("g<M-e>", "<Plug>(sexp_move_to_prev_element_tail)", { desc = "Sexp move to prev element tail" })
  xmap("g<M-e>", "<Plug>(sexp_move_to_prev_element_tail)", { desc = "Sexp move to prev element tail" })
  omap("g<M-e>", "<Plug>(sexp_move_to_prev_element_tail)", { desc = "Sexp move to prev element tail" })

  nmap("[e", "<Plug>(sexp_select_prev_element)", { desc = "Sexp select prev element" })
  xmap("[e", "<Plug>(sexp_select_prev_element)", { desc = "Sexp select prev element" })
  omap("[e", "<Plug>(sexp_select_prev_element)", { desc = "Sexp select prev element" })
  nmap("]e", "<Plug>(sexp_select_next_element)", { desc = "Sexp select next element" })
  xmap("]e", "<Plug>(sexp_select_next_element)", { desc = "Sexp select next element" })
  omap("]e", "<Plug>(sexp_select_next_element)", { desc = "Sexp select next element" })
  nmap("==", "<Plug>(sexp_indent)", { desc = "Sexp indent" })
  nmap("=-", "<Plug>(sexp_indent_top)", { desc = "Sexp indent top" })

  -- TODO: TMUX tilish plugin uses the same keybindings and it's getting in the way, consuming the event.
  nmap("<M-k>", "<Plug>(sexp_swap_list_backward)", { desc = "Sexp swap list backward" })
  xmap("<M-k>", "<Plug>(sexp_swap_list_backward)", { desc = "Sexp swap list backward" })
  nmap("<M-j>", "<Plug>(sexp_swap_list_forward)", { desc = "Sexp swap list forward" })
  xmap("<M-j>", "<Plug>(sexp_swap_list_forward)", { desc = "Sexp swap list forward" })
  nmap("<M-h>", "<Plug>(sexp_swap_element_backward)", { desc = "Sexp swap element backward" })
  xmap("<M-h>", "<Plug>(sexp_swap_element_backward)", { desc = "Sexp swap element backward" })
  nmap("<M-l>", "<Plug>(sexp_swap_element_forward)", { desc = "Sexp swap element forward" })
  xmap("<M-l>", "<Plug>(sexp_swap_element_forward)", { desc = "Sexp swap element forward" })

  imap("<BS>", "<Plug>(sexp_insert_backspace)", { desc = "Sexp insert backspace" })
  imap('"', "<Plug>(sexp_insert_double_quote)", { desc = "Sexp insert double quote" })
  imap("(", "<Plug>(sexp_insert_opening_round)", { desc = "Sexp insert opening round" })
  imap(")", "<Plug>(sexp_insert_closing_round)", { desc = "Sexp insert closing round" })
  imap("[", "<Plug>(sexp_insert_opening_square)", { desc = "Sexp insert opening square" })
  imap("]", "<Plug>(sexp_insert_closing_square)", { desc = "Sexp insert closing square" })
  imap("{", "<Plug>(sexp_insert_opening_curly)", { desc = "Sexp insert opening curly" })
  imap("}", "<Plug>(sexp_insert_closing_curly)", { desc = "Sexp insert closing curly" })

  nmap("<LocalLeader>s(", "<Plug>(sexp_round_head_wrap_list)", { desc = "Sexp round head wrap list" })
  xmap("<LocalLeader>s(", "<Plug>(sexp_round_head_wrap_list)", { desc = "Sexp round head wrap list" })
  nmap("<LocalLeader>s)", "<Plug>(sexp_round_tail_wrap_list)", { desc = "Sexp round tail wrap list" })
  xmap("<LocalLeader>s)", "<Plug>(sexp_round_tail_wrap_list)", { desc = "Sexp round tail wrap list" })
  nmap("<LocalLeader>s[", "<Plug>(sexp_square_head_wrap_list)", { desc = "Sexp square head wrap list" })
  xmap("<LocalLeader>s[", "<Plug>(sexp_square_head_wrap_list)", { desc = "Sexp square head wrap list" })
  nmap("<LocalLeader>s]", "<Plug>(sexp_square_tail_wrap_list)", { desc = "Sexp square tail wrap list" })
  xmap("<LocalLeader>s]", "<Plug>(sexp_square_tail_wrap_list)", { desc = "Sexp square tail wrap list" })
  nmap("<LocalLeader>s{", "<Plug>(sexp_curly_head_wrap_list)", { desc = "Sexp curly head wrap list" })
  xmap("<LocalLeader>s{", "<Plug>(sexp_curly_head_wrap_list)", { desc = "Sexp curly head wrap list" })
  nmap("<LocalLeader>s}", "<Plug>(sexp_curly_tail_wrap_list)", { desc = "Sexp curly tail wrap list" })
  xmap("<LocalLeader>s}", "<Plug>(sexp_curly_tail_wrap_list)", { desc = "Sexp curly tail wrap list" })

  nmap("<LocalLeader>sb", "<Plug>(sexp_emit_tail_element)", { desc = "Barf forward" })
  xmap("<LocalLeader>sb", "<Plug>(sexp_emit_tail_element)", { desc = "Barf forward" })

  nmap("<LocalLeader>ss", "<Plug>(sexp_capture_next_element)", { desc = "Slurp forward" })
  xmap("<LocalLeader>ss", "<Plug>(sexp_capture_next_element)", { desc = "Slurp forward" })

  nmap("<LocalLeader>sI", "<Plug>(sexp_insert_at_list_head)", { desc = "Insert at head" })
  nmap("<LocalLeader>sA", "<Plug>(sexp_insert_at_list_tail)", { desc = "Insert at tail" })

  nmap("<LocalLeader>s@", "<Plug>(sexp_splice_list)", { desc = "Sexp splice list" })
end

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "clojure" },
  callback = vim_sexp_mappings,
})

return {
  "guns/vim-sexp",

  ft = { "clojure", "lisp", "fennel", "scheme", "janet" },

  init = function()
    vim.g.sexp_filetypes = "" --"clojure,scheme,lisp,fennel,janet"
  end,

  dependencies = {
    "radenling/vim-dispatch-neovim",
    "tpope/vim-sexp-mappings-for-regular-people",
    "tpope/vim-repeat",
  },
}
