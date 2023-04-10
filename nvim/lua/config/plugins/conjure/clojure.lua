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

	local conjure_taps = {
		eval_tap_word = function()
			local word = conjure_word()
			conjure_eval_tap(word)
		end,

		eval_tap_form = function()
			local form = conjure_form(false)
			conjure_eval_tap(form)
		end,

		eval_tap_root_form = function()
			local form = conjure_form(true)
			conjure_eval_tap(form)
		end,

		tap_last_exception = conjure_eval_fn("(tap> (Throwable->map *e))"),

		tap_ns_publics = conjure_eval_fn("(tap> (ns-publics *ns*))"),
	}

	return {
		p = {
			name = "+conjure",
			e = { conjure_taps.eval_tap_word, "Eval word" },
		},
	}
end

-- local config = require("conjure.config")
-- local client = require("conjure.client")

-- nvim.ex.function_(str.join("\n", {"ConjureEvalMotionOpFunc(kind)", "call luaeval(\"require('conjure.eval')['selection'](_A)\", a:kind)", "endfunction"}))

return M
