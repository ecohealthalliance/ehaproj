#' Like dotenv::load_dot_env but allows external setting to override if desired
#'
#' Also checks if file is binary and/or encrypted and reports a message back.
#' @param files Files to load environment variables from, The default, [env_files()],
#' loads all files in the working directory starting with `.env*`, in alphabetical
#' order
#' @param override Whether variables in `.env*` should override already-set
#' environment variables. The default, "exported" only does this for variables
#' prefixed with `export`.
load_dotenv <- function(files = env_files(), override = c("exported", "all", "none"),
                        warning_on_error = TRUE) {
  NULL
}


#' List of files starting with `.env`
#' @param path directory to search in
#' @param regexp matching regular expression
#' @param ... other arguments to [fs::dir_ls()]
env_files <- function(path = ".", regexp = "^\\.env[^/]*$", ...) {
  sort(fs::dir_ls(path = path, regexp = regexp, ...))
}


