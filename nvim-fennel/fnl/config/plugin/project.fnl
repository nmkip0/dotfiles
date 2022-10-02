(module config.plugin.project
  {autoload {project project_nvim
             telescope telescope}})

(project.setup
  { :patterns ["deps.edn"] })

(telescope.load_extension :projects)

(comment 
  (require :project_nvim.config)

  nil)

