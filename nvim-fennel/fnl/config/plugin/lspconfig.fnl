(module config.plugin.lspconfig
  {autoload {nvim aniseed.nvim
             lsp lspconfig
             tb telescope.builtin
             util lspconfig.util
             cmplsp cmp_nvim_lsp
             wk which-key
             fzf fzf-lua
             conjure conjure}})

;symbols to show for lsp diagnostics
(defn define-signs
  [prefix]
  (let [error (.. prefix "SignError")
        warn  (.. prefix "SignWarn")
        info  (.. prefix "SignInfo")
        hint  (.. prefix "SignHint")]
  (vim.fn.sign_define error {:text "" :texthl error})
  (vim.fn.sign_define warn  {:text "" :texthl warn})
  (vim.fn.sign_define info  {:text "" :texthl info})
  (vim.fn.sign_define hint  {:text "" :texthl hint})))

(define-signs "LspDiagnostics")

(defn create-hl-group [bufnr]
  (let [group :lsp_document_highlight
        hl-events [:CursorHold :CursorHoldI]
        (ok hl-autocmds) (pcall nvim.get_autocmds {:group group 
                                                   :buffer bufnr
                                                   :event hl-events})]
    (when (or (not ok) (= (length hl-autocmds) 0))
      (nvim.create_augroup group {:clear false})
      (nvim.create_autocmd :CursorHold
                           {:group group
                            :buffer bufnr
                            :callback vim.lsp.buf.document_highlight})
      (nvim.create_autocmd :CursorHoldI
                           {:group group
                            :buffer bufnr
                            :callback vim.lsp.buf.document_highlight})
      (nvim.create_autocmd :CursorMoved
                           {:group group
                            :buffer bufnr
                            :callback vim.lsp.buf.clear_references}))))

(defn setup-document-highlight [client bufnr]
  (let [(status-ok highlight-supported) 
        (pcall #(client.supports_method :textDocument/documentHighlight))]
    (when (and status-ok highlight-supported)
      (create-hl-group bufnr))))

(vim.diagnostic.config {:virtual_lines {:only_current_line true}})

(let [handlers {"textDocument/publishDiagnostics"
                (vim.lsp.with
                  vim.lsp.diagnostic.on_publish_diagnostics
                  {:virtual_text true
                   :signs true
                   :underline true
                   :update_in_insert false
                   :severity_sort false})
                "textDocument/hover"
                (vim.lsp.with
                  vim.lsp.handlers.hover
                  {:border "single"})
                "textDocument/signatureHelp"
                (vim.lsp.with
                  vim.lsp.handlers.signature_help
                  {:border "single"})}

      capabilities (cmplsp.update_capabilities
                     (vim.lsp.protocol.make_client_capabilities))
      bindings 
      {:g {:d [tb.lsp_definitions "Go to definition"]
           :r [tb.lsp_references "LSP rerefences"]
           :t [tb.lsp_type_definitions "Type definition"]}
       :<leader> {:l {:r [vim.lsp.buf.rename "Rename"]
                      :a [vim.lsp.buf.code_action "Code actions"]
                      :s [tb.lsp_document_symbols "Document symbols"]
                      :S [tb.lsp_dynamic_workspace_symbols "Workspace symbols"]
                      :f [vim.lsp.buf.format "Format buffer"]
                      :d [tb.diagnostics "Document diagnostics"]
                      :R [":LspRestart<cr>" "Restart LSP"]}}
       :K [vim.lsp.buf.hover "Hover doc"]}
      on_attach (fn [client bufnr]
                  (setup-document-highlight client bufnr)
                  (wk.register bindings {:noremap true
                                         :buffer bufnr}))]

  ;; Clojure
  (lsp.clojure_lsp.setup {:on_attach on_attach
                          :handlers handlers
                          :cmd ["/usr/bin/clojure-lsp"]
                          :capabilities capabilities})

  ;; JavaScript and TypeScript
  ;; (lsp.tsserver.setup {:on_attach on_attach
  ;;                      ;; :handlers handlers
  ;;                      :capabilities capabilities})
  ;;
  ;; ;; html / css / json
  ;; (lsp.cssls.setup {:on_attach on_attach
  ;;                   ;; :handlers handlers
  ;;                   :capabilities capabilities
  ;;                   :cmd ["vscode-css-languageserver" "--stdio"]})
  ;;
  ;; (lsp.html.setup {:on_attach on_attach
  ;;                  ;; :handlers handlers
  ;;                  :capabilities capabilities
  ;;                  :cmd ["vscode-html-languageserver" "--stdio"]})
  ;;
  ;; (lsp.jsonls.setup {:on_attach on_attach
  ;;                    ;; :handlers handlers
  ;;                    :capabilities capabilities
  ;;                    :cmd ["vscode-json-languageserver" "--stdio"]})
  )

