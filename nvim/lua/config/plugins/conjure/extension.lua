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
      conjure_eval(form, passive or false)
    end
  end

  local function conjure_eval_tap(form)
    conjure_eval("(doto " .. form .. " tap>)")
  end

  local function conjure_word()
    return extract.word().content
  end

  local function conjure_form(is_root)
    return extract.form({ ["root?"] = is_root }).content
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

  return {
    eval = conjure_eval_fn,
    tap_word = eval_tap_word,
    tap_form = eval_tap_form,
    tap_root_form = eval_tap_root_form,
    tap_last_exception = eval_tap_root_form,
    tap_ns_publics = eval_tap_root_form,

    mappings = {
      p = {
        name = "+conjure",
        w = { eval_tap_word, "Tap word" },
        f = { eval_tap_form, "Tap form" },
        r = { eval_tap_root_form, "Tap root form" },

        E = { tap_last_exception, "Tap last exception" },
        N = { tap_ns_publics, "Tap ns publics" },
      },
    },
  }
end

return M
