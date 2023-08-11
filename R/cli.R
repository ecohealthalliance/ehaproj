# Utility functions for printing messages and prompts

#From https://github.com/r-lib/cli/issues/488
cli_ask <- function(text, default = NA, .envir = parent.frame()) {
  deftxt <- if(is.na(default)) "" else paste0("(Default: ", default, ")")
  bullet(paste(text, deftxt), .envir = .envir)
  res <- prompt()
  if(res == "") res <- default
  res
}

cli_menu <- function(text, choices, default = NA, .envir = parent.frame()) {
  if (is.character(default)) default <- pmatch(default, choices)
  default <- as.numeric(default)
  stopifnot(is.na(default) || is_index(choices, default))
  deftxt <- if(is.na(default)) "" else paste0("(Default: ", default, ". ", choices[default], ")")
  repeat {
    cli_div(theme = list(ol = list("margin-left" = 2)))
    bullet(text, .envir = .envir)
    cli_ol(choices)
    cli_end()
    res <- prompt()
    if (res == "" && !is.na(default)) {
      res <- default
      break
    }
    suppressWarnings(res <- as.numeric(res))
    if (!is_index(res, choices)) {
      cli_alert_warning("Sorry, I did not get that.")
    } else {
      res <- choices[res]
      break
    }
  }
  res
}

cli_yesno <- function(text, default = NA, .envir = parent.frame()) {
  deftxt <- if(is.na(default)) "(y/n)" else c(" (y/N) ", " (Y/n) ")[default + 1]
  repeat {
    bullet(paste(text, deftxt), .envir = .envir)
    ans <- prompt()
    res <- NA
    if (ans == "") res <- default
    if (grepl("^\\s*(y|yes|t|true)\\s*$", response, ignore.case = TRUE)) res <- TRUE
    if (grepl("^\\s*(n|no|f|false)\\s*$", response, ignore.case = TRUE)) res <- FALSE
    if (!is.na(res)) break
    cli_alert_warning("Sorry, I did not get that.")
  }
  res
}

is_index <- function(vector, idx) {
  !is.na(idx) && is_scalar_integerish(idx) &&
    idx >= 1 && idx <= length(vector)
}

prompt <- function(bullet = ">") {
  readline(cli_fmt(cli_bullets(set_names("", bullet))))
}

bullet <- function(text, bullet = "*", .envir = parent.frame()) {
  cli_bullets(set_names(text, bullet), .envir = .envir)
}

ehaproj_verbose <- function() {
  verbose <- getOption("ehaproj.verbose")
  if (!is.null(verbose))
    return(as.logical(verbose))

  verbose <- Sys.getenv("EHAPROJ_VERBOSE", unset = NA)
  if (!is.na(verbose))
    return(as.logical(verbose))

  if (is_testing())
    return(FALSE)

  interactive()

}

vb <- function(expr) {
  if(ehaproj_verbose()) eval(expr, envir = parent.frame(n = 1))
  invisible(NULL)
}



