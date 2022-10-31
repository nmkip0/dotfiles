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

local portal_cmds = {
  open = conjure_eval_fn [[
    (def portal 
      ((requiring-resolve 'portal.api/open)
        {:theme :portal.colors/nord})) 
    ]],

  add_tap = conjure_eval_fn "(add-tap (requiring-resolve 'portal.api/submit))",
  remove_tap = conjure_eval_fn "(remove-tap (requiring-resolve 'portal.api/submit))",

  clear = conjure_eval_fn "(portal.api/clear)",

  last_exception = conjure_eval_fn "(tap> (Throwable->map *e))",

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
}

local portal_mappings = {
  p = {
    name = "+portal",
    o = { portal_cmds.open, "Portal open" },
    c = { portal_cmds.clear, "Portal clear" },

    e = { portal_cmds.last_exception, "Portal last exception" },
    w = { portal_cmds.tap_word, "Portal tap word" },
    f = { portal_cmds.tap_form, "Portal tap form" },
    r = { portal_cmds.tap_root_form, "Portal tap root form" },

    R = { portal_cmds.remove_tap, "Remove tap"},
    T = { portal_cmds.add_tap, "Add tap"}
  }
}

return portal_mappings
