local eval_ok, eval = pcall(require, "conjure.eval")
if not eval_ok then
  return
end

local extract_ok, extract = pcall(require, "conjure.extract")
if not extract_ok then
  return
end

local function conjure_eval(form)
  eval["eval-str"]({ code = form, origin = "custom_command" })
end

local function conjure_eval_fn(form)
  return function()
    conjure_eval(form)
  end
end

local function conjure_word()
  return extract.word().content
end

local function conjure_form(is_root)
  return extract.form({ ["root?"] = is_root }).content
end

local function invoke_portal_command(command)
  return conjure_eval_fn(
    [[
    (do
      (#?(:clj portal.api/eval-str :cljs portal.web/eval-str) "]]
      .. command ..
    [[
      ")
    )]]
  )
end

local conjure_taps = {
  tap_word = function()
    local word = conjure_word()
    conjure_eval("(tap> " .. word .. ")")
  end,

  tap_form = function()
    local form = conjure_form(false)
    conjure_eval("(tap> " .. form .. ")")
  end,

  tap_root_form = function()
    local form = conjure_form(true)
    conjure_eval("(tap> " .. form .. ")")
  end,

  last_exception = conjure_eval_fn "(tap> (Throwable->map *e))",
}

local portal_cmds = {
  open = conjure_eval_fn [[
    (def portal 
      ((requiring-resolve 'portal.api/open)
        {:theme :portal.colors/nord})) 
    ]],

  add_tap = conjure_eval_fn "(add-tap (requiring-resolve 'portal.api/submit))",
  remove_tap = conjure_eval_fn "(remove-tap (requiring-resolve 'portal.api/submit))",
  clear = conjure_eval_fn "(portal.api/clear)",
  next_viewer = invoke_portal_command "(portal.ui.commands/select-next-viewer portal.ui.state/state)",
  prev_viewer = invoke_portal_command "(portal.ui.commands/select-prev-viewer portal.ui.state/state)",
  select_root = invoke_portal_command "(portal.ui.commands/select-root portal.ui.state/state)",
  select_next = invoke_portal_command "(portal.ui.commands/select-next portal.ui.state/state)",
  select_prev = invoke_portal_command "(portal.ui.commands/select-prev portal.ui.state/state)",
  select_parent = invoke_portal_command "(portal.ui.commands/select-parent portal.ui.state/state)",
  select_child = invoke_portal_command "(portal.ui.commands/select-child portal.ui.state/state)",
  history_back = invoke_portal_command "(portal.ui.commands/history-back portal.ui.state/state)",
  history_forward = invoke_portal_command "(portal.ui.commands/history-forward portal.ui.state/state)",
  focus_selected = invoke_portal_command "(portal.ui.commands/focus-selected portal.ui.state/state)",
  toggle_expand = invoke_portal_command "(portal.ui.commands/toggle-expand portal.ui.state/state)",
}

local portal_mappings = {
  p = {
    name = "+portal",
    o = { portal_cmds.open, "Portal open" },
    c = { portal_cmds.clear, "Portal clear" },

    e = { conjure_taps.last_exception, "Portal last exception" },
    w = { conjure_taps.tap_word, "Portal tap word" },
    f = { conjure_taps.tap_form, "Portal tap form" },
    r = { conjure_taps.tap_root_form, "Portal tap root form" },

    R = { portal_cmds.remove_tap, "Remove tap"},
    T = { portal_cmds.add_tap, "Add tap"},
    w = { portal_cmds.toggle_expand, "Toggle expand"},
    x = { portal_cmds.select_root, "Select root"},
    y = { portal_cmds.select_next, "Select next"},
    z = { portal_cmds.select_prev, "Select prev"}
  }
}

return portal_mappings
