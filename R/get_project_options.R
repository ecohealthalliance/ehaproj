get_parent_directory <- function() {

  default_dir <- fs::path_home()
  canpick <- interactive() && rstudioapi::isAvailable() && rstudioapi::getVersion() > '1.1.287'
  prompt <- "Parent directory for project folder"
  if(canpick) {
    prompt <- paste(prompt, "(enter 'i' to select interactively)")
    default <- "i"
  } else {
    default = path_home()
  }
  parent_dir <- cli_ask(prompt, default)

  if(canpick && parent_dir == "i") {
    parent_dir <- rstudioapi::selectDirectory("Select the directory where you will put your project folder", path = default_dir)
  }

}
