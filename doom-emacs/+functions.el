(defun jet-pretty ()
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "jet --pretty --edn-reader-opts '{:default tagged-literal}'"
   (current-buffer)
   t
   "*jet error buffer*"
   t))

(defun jet-pretty-region (beg end)
  (shell-command-on-region
   beg
   end
   "jet --pretty --edn-reader-opts '{:default tagged-literal}'"
   (current-buffer)
   t
   "*jet error buffer*"
   t))

(defun jet-pretty-json-region (beg end)
  (shell-command-on-region
   beg
   end
   "jet --to json --pretty --edn-reader-opts '{:default tagged-literal}'"
   (current-buffer)
   t
   "*jet error buffer*"
   t))

(defun jet-edn-region (beg end)
  (interactive)

  (shell-command-on-region
   beg
   end
   "jet --to edn --edn-reader-opts '{:default tagged-literal}'"
   (current-buffer)
   t
   "*jet error buffer*"
   t))
