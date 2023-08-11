recommended_addin_shortcuts <- function() {
  list(`tflow::tflow_load_all` = "Ctrl+Alt+P",
       `targets::rstudio_addin_tar_load` = "Ctrl+Alt+L",
       `fnmate::rs_fnmate` = "Ctrl+Alt+F",
       `tflow::rs_make_target_at_cursor` = "Ctrl+Alt+M",
       `tflow::rs_load_current_editor_targets` =  "Ctrl+Alt+;")
}

rs_config_path <- function(os = get_os()) {
  if (os == "windows")
    file.path(Sys.getenv("USERPROFILE"), "AppData", "Roaming", "RStudio")
  else
    file.path(normalizePath(file.path("~/", ".config", "rstudio")))
}

rs_shortcuts_paths <- function(
    types = c("addins", "editor_bindings", "rstudio_bindings"),
    config_path = rs_config_path()) {
  set_names(file.path(config_path, "keybindings", paste0(types, ".json")), types)
}

rs_shortcuts <- function(paths = rs_shortcuts_paths()) {
  sc <- lapply(paths, function(path) {
    if(file.exists(path))
      jsonlite::fromJSON(path)
    else {
      list()
    }
  })
  sc <- do.call(c, sc)
}

all_shortcuts <- function(config_path = rs_config_path()) {
   rs_addin_shortcuts

}

  # new <- setdiff(names(shortcuts), names(kb))
  # if(length(new)) {
  #   kb <- purrr::list_modify(kb, !!!shortcuts)
  #   jsonlite::write_json(kb, kb_addin_path, auto_unbox = TRUE)
  #   new <- unlist(kb[new])
  #   message("• New shortcuts: ", paste0(new, ": ", names(new), collapse = ", "))
  # }
