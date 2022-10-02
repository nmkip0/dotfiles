(module config.packer
  {autoload {nvim aniseed.nvim
             a aniseed.core
             packer packer }})

(defn safe-require-plugin-config! 
  [name]
  (let [(ok? val-or-err) (pcall require (.. :config.plugin. name))]
    (when (not ok?)
      (print (.. "config error: " val-or-err)))))


(defn use!
  [pkgs]
  "Iterates through the arguments as pairs and calls packer's use function for
  each of them. Works around Fennel not liking mixed associative and sequential
  tables as well."
  (packer.startup
      (fn [use]
        (a.run! 
          (fn [[name opts]]
            (-?> (. opts :mod) (safe-require-plugin-config!))
            (use (a.assoc opts 1 name)))
          (a.kv-pairs pkgs))))
  nil)

