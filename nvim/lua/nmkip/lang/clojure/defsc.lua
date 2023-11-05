local M = {}

local eval = require("conjure.eval")

function M.defsc()
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

return M
