local eval_ok, eval = pcall(require, "conjure.eval")
if not eval_ok then
  return
end

local extract_ok, extract = pcall(require, "conjure.extract")
if not extract_ok then
  return
end

local function conjure_eval(form, passive)
  print("Passive? ", passive)
  eval["eval-str"]({
    code = form,
    origin = "custom_command",
    ["passive?"] = passive
  })
end

local function conjure_eval_fn(form, passive)
  return function()
    conjure_eval(form, passive or false)
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
    )]],
    true
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

local stdout_to_clipboard = function (_, data)
  local result = ""
  for _, value in ipairs(data) do
    result = result .. value .. "\n"
  end
  vim.call("setreg", "+", result)
end

local portal_copy = function (parse_cmd)
  eval["eval-str"]({
    code = "@user/portal",
    origin = "custom_command",
    ["passive?"] = true,
    ["on-result"] = function (r)
      vim.fn.jobstart(parse_cmd(r),
        {
          stdout_buffered = true,
          on_stdout = stdout_to_clipboard
        })
    end
  })
end

local portal_copy_json = function ()
  local command = function (result)
    return "echo '" .. result .. "' | jet --to json --pretty --edn-reader-opts '{:default tagged-literal}'"
  end
  portal_copy(command)
end

local portal_copy_edn = function ()
  local command = function (result)
    return "echo '" .. result .. "' | jet  --edn-reader-opts '{:default tagged-literal}'"
  end
  portal_copy(command)
end


local def_selected = function ()
  local var_name = nil
  vim.ui.input({prompt = "Enter var name: "}, function (input)
      -- TODO: Check clojure syntax
      var_name = vim.trim(input)
    end)

  if var_name then
    eval["eval-str"]({
      code = "(def " .. var_name .. " @user/portal)",
      origin = "custom_command",
      ["passive?"] = false,
    })
  end
end

local portal_cmds = {
  open = conjure_eval_fn [[
    (in-ns 'user)
    (def portal 
      ((requiring-resolve 'portal.api/open)
        {:theme :portal.colors/nord}))
    ]],

  add_tap = conjure_eval_fn "(add-tap (requiring-resolve 'portal.api/submit))",
  remove_tap = conjure_eval_fn "(remove-tap (requiring-resolve 'portal.api/submit))",
  clear = conjure_eval_fn "(portal.api/clear)",
  copy_json = portal_copy_json,
  copy_edn = portal_copy_edn,
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
  def_selected = def_selected,
}
local hydra_ok, hydra = pcall(require, "hydra")
if not hydra_ok then
  return
end

local portal_hydra = hydra({
   name = 'Portal',
   mode = 'n',
   heads = {
      { '1', portal_cmds.copy_json, { desc = 'Copy json'}},
      { '2', portal_cmds.copy_edn , { desc = 'Copy edn'}},
      { 'e', portal_cmds.toggle_expand, { desc = 'Toggle expand'}},
      { 'h', portal_cmds.select_parent },
      { 'j', portal_cmds.select_next },
      { 'k', portal_cmds.select_prev },
      { 'l', portal_cmds.select_child },
      { 'r', portal_cmds.select_root },
      { 'J', portal_cmds.next_viewer },
      { 'K', portal_cmds.prev_viewer },
      { 'V', portal_cmds.def_selected },
      { '<C-h>', portal_cmds.history_back },
      { '<C-l>', portal_cmds.history_forward },
      { '<CR>', portal_cmds.focus_selected },
      { '<C-k>', portal_cmds.clear },
   }
})

local function portal_mode_display ()
  return portal_hydra:activate()
end

local portal_mappings = {
  p = {
    name = "+portal",
    ["."] = { portal_mode_display , "Portal mode"},
    o = { portal_cmds.open, "Portal open" },
    c = { portal_cmds.clear, "Portal clear" },

    e = { conjure_taps.last_exception, "Portal last exception" },
    w = { conjure_taps.tap_word, "Portal tap word" },
    f = { conjure_taps.tap_form, "Portal tap form" },
    r = { conjure_taps.tap_root_form, "Portal tap root form" },

    R = { portal_cmds.remove_tap, "Remove tap"},
    T = { portal_cmds.add_tap, "Add tap"},
    V = { portal_cmds.def_selected, "def selected"}
  }
}

return portal_mappings
