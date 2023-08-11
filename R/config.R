#' Project configuration
#'
#' `ehaproj` lets you set preferences for default new project configuration in
#' several different ways:
#'
#' 1. If an environment variable of the form `EHAPROJ_CONFIG_<NAME>` is available,
#'   then that option's value will be used;
#'
#' 2. If an \R option of the form `ehaproj.<name>` is available,
#'    then that option's value will be used;
#'
#' 2. If the option is set in the user configuration YAML file, found at
#'    `ehaproj_config_path()`. This file is modified if the user opts to save
#'    settings as defaults when creating a project. You can edit it with
#'    `edit_ehaproj_config()`;
#'
#' 4. Otherwise, the default for that particular configuration value is used.
#'    You can see these defaults wiht `ehaproj_config(defaults = TRUE)`
#'
#' For example, the
#' configuration option `parent_dir` could be configured as:
#'
#' - `options(ehaproj.parent_dir = <...>)`
#' - `Sys.setenv(EHAPROJ_PARENT_DIR = <...>)`
#' - `parent_dir: <...>` in the configuration YAML file.
#'
#' If configration options are prepended with `ask:` (as in `use_git: "ask:TRUE"`),
#' the user will be prompted for an answer with the option being provided as
#' a default.  Several options can be set to `"i"`, in which case `ehaproj()`
#' will launch interactive dialogs for picking from options.
#'
# #' @eval ehaproj_roxygen_config_section()
#'
#' @examples
#'
#' # set your default directory for projects
#' options(ehaproj.parent_dir = "~/projects")
#'
#'#' # set your default github namespace
#' Sys.setenv(EHAPROJ_GITHUB_ORG = "ecohealthalliance")
#'
#' # always use git-crypt in projects
#' Sys.setenv(EHAPROJ_USE_CRYPT = TRUE)
#'
#' @param ... name-value pairs of configuration options to set
#' @param .options name-value pairs of configuration options, as list
#' @param defaults Return package defaults, unmodified by any user config,
#'   options, or variables
#' @rdname config
#' @name config
#' @importFrom purrr map_chr compact discard
ehaproj_config <- function(..., .options = list(), defaults = FALSE) {

  opts <- ehaproj_config_info()
  opt_names <- map_chr(opts, \(x) x[["name"]])
  pkg_config <- set_names(map(opts, \(x) x[["default"]]),
                       nm = map_chr(opts, \(x) x[["name"]]))

  class(pkg_config) <- "ehaproj_config"

  if(defaults) return(pkg_config)

  fun_config <- c(list2(...), .options)
  usr_config <- ehaproj_user_config()
  opt_config <- map(
    paste0("ehaproj.", tolower(names(pkg_config))),
    \(x) getOption(x, default = NULL)
  ) |>
    set_names(opt_names) |>
    compact()
  env_config <- map(
    paste0("EHAPROJ", toupper(names(pkg_config)), sep = "_"),
    \(x) var = Sys.getenv(x, unset = "")
  ) |>
    set_names(opt_names) |>
    discard(\(x) x == "")

  config <- pkg_config |>
    modifyList(usr_config) |>
    modifyList(opt_config) |>
    modifyList(env_config) |>
    modifyList(fun_config)


  config
}

ehaproj_config_decode_envvar <- function(envname, envval) {

  map <- env(
    "null" = NULL,
    "NULL" = NULL,
    "na"   = NA,
    "NA"   = NA,
    "NaN"  = NaN,
    "true" = TRUE,
    "True" = TRUE,
    "TRUE" = TRUE,
    "false" = FALSE,
    "False" = FALSE,
    "FALSE" = FALSE
  )

  if (exists(envval, envir = map, inherits = FALSE))
    return(get(envval, envir = map, inherits = FALSE))


  # Note the below if we need to split the variable along something besides commas
  # libvars <- c("RENV_CONFIG_EXTERNAL_LIBRARIES", "RENV_CONFIG_HYDRATE_LIBPATHS")
  # pattern <- if (envname %in% libvars)
  #   "\\s*[:;,]\\s*"
  # else
  #   "\\s*,\\s*"

  pattern <- "\\s*,\\s*"

  strsplit(envval, pattern, perl = TRUE)[[1L]]

}

ehaproj_config_validate <- function(name, value, type, default, args) {

  # no validation required for type = '*'
  if (identical(type, "*"))
    return(value)

  # if 'value' is a function, invoke it with args
  if (is.function(value)) {
    value <- catch(do.call(value, args))
    if (inherits(value, "error")) {
      warning(value, call. = FALSE)
      return(default)
    }
  }

  # parse the type string
  pattern <- paste0(
    "^",          # start of specifier
    "([^[(]+)",   # type name
    "[[(]",       # opening bracket
    "([^])]+)",   # length specifier
    "[])]",       # closing bracket
    "$"           # end of specifier
  )

  m <- regexec(pattern, type)
  matches <- regmatches(type, m)
  fields <- matches[[1L]]

  # extract declared mode, size
  mode <- fields[[2L]]
  size <- fields[[3L]]

  # validate the requested size for this option
  if (!ehaproj_config_validate_size(value, size)) {
    fmt <- "value for option '%s' does not satisfy constraint '%s'"
    warningf(fmt, name, type)
  }

  # convert NULL values to requested type
  if (is.null(value)) {
    value <- convert(value, mode)
    return(value)
  }

  # otherwise, validate that this is a valid option
  if (identical(storage.mode(value), mode))
    return(value)

  # try converting
  converted <- catchall(convert(value, mode))
  if (any(is.na(converted)) || inherits(converted, "condition")) {
    fmt <- "'%s' does not satisfy constraint '%s' for config '%s'; using default '%s' instead"
    warningf(fmt, stringify(value), type, name, stringify(default))
    return(default)
  }

  # ok, validated + converted option
  converted

}

ehaproj_config_validate_size <- function(value, size) {

  case(
    size == "*" ~ TRUE,
    size == "+" ~ length(value) > 0,
    size == "?" ~ length(value) %in% c(0, 1),
    TRUE        ~ as.numeric(size) == length(value)
  )

}

#' @export
#' @rdname config
#' @importFrom tools R_user_dir
ehaproj_config_path <- function() {
  Sys.getenv(
    "EHAPROJ_CONFIG_PATH",
    unset = path(R_user_dir("ehaproj", "config"), "config.yml"))
}

#' @importFrom yaml read_yaml
ehaproj_user_config <- function(path = config_path()) {
  user_config <- maybe(read_yaml)(path)
  if(!is.list(user_config)) user_config <- list()

  class(user_config) <- "ehaproj_config"
  user_config

}

#' @export
#' @rdname config
edit_ehaproj_config <- function() {
  usethis::edit_file(config_path())
}

ehaproj_config_info <- function() {
  info <- yaml::read_yaml(system.file("config.yml", package = "ehaproj"))
  names(info) <- map_chr(info, \(x) x[["name"]])
  info
}
