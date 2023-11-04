local M = {}

function M.config()
  local extract = require("conjure.extract")
  local eval = require("conjure.eval")

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

  local function eval_tap_word()
    local word = conjure_word()
    conjure_eval_tap(word)
  end

  local function eval_tap_form()
    local form = conjure_form(false)
    conjure_eval_tap(form)
  end

  local function eval_tap_root_form()
    local form = conjure_form(true)
    conjure_eval_tap(form)
  end

  local tap_last_exception = conjure_eval_fn("(tap> (Throwable->map *e))")

  local tap_ns_publics = conjure_eval_fn("(tap> (ns-publics *ns*))")

  local function scope_capture_defsc()
    local id = nil
    vim.ui.input({ prompt = "Enter id: " }, function(input)
      id = vim.trim(input)
    end)

    if id then
      eval["eval-str"]({
        code = "(sc.api/defsc " .. id .. ")",
        origin = "custom_command",
        ["passive?"] = false,
      })
    end
  end

  return {
    eval = conjure_eval_fn,
    eval_tap_word = eval_tap_word,
    eval_tap_form = eval_tap_form,
    eval_tap_root_form = eval_tap_root_form,
    eval_tap_last_exception = eval_tap_root_form,
    eval_tap_ns_publics = eval_tap_root_form,
    scope_capture_defsc = scope_capture_defsc,

    mappings = {
      e = {
        name = "+eval",
        w = { eval_tap_word, "Eval word" },
        f = { eval_tap_form, "Eval form" },
        r = { eval_tap_root_form, "Eval root form" },

        E = { tap_last_exception, "Eval last exception" },
        N = { tap_ns_publics, "Eval ns publics" },
      },
      R = {
        name = "+repl",
        C = { scope_capture_defsc, "api.sc/defsc" },
      },
    },
  }
end

return M
