local M = {}

local extract = require("conjure.extract")
local eval = require("conjure.eval")

function M.eval(ns, code, opts)
  opts = opts or {}

  local client = require("conjure.client")
  local fn = require("conjure.eval")["eval-str"]

  client["with-filetype"](
    "clojure",
    fn,
    vim.tbl_extend("force", {
                     origin = "nmkip.lang.clojure.eval",
                     context = ns,
                     code = code,
                            }, opts)
                         )
end

local function conjure_eval(form, passive)
  return eval["eval-str"]({
      code = form,
      origin = "custom_command",
      ["passive?"] = passive,
                         })
end

local function conjure_eval_fn(form, passive)
  return function()
    if form then
      conjure_eval(form, passive or false)
    end
  end
end

local function conjure_eval_tap(form)
  if form then
    conjure_eval("(doto " .. form .. " tap>)")
  end
end

local function conjure_word()
  return extract.word().content
end

local function conjure_form(is_root)
  local form = extract.form({ ["root?"] = is_root })
  return form and form.content
end

function M.eval_tap_word()
  local word = conjure_word()
  conjure_eval_tap(word)
end

function M.eval_tap_form()
  local form = conjure_form(false)
  conjure_eval_tap(form)
end

function M.eval_tap_root_form()
  local form = conjure_form(true)
  conjure_eval_tap(form)
end

return M
