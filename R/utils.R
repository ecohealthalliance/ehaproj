#' Determine the operating system
get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

# Wrap stuff in a common class
ec <- function(x, addclass = NULL, baseclass = "ehaproj") {
  class(x) <- c(addclass, baseclass, class(x))
  x
}

print.ehaproj <- function(x, ...) {
  cat(format(x, ...))
}


obfuscate <- function (x, first = 4, last = 4) {
  paste0(substr(x, start = 1, stop = first), "...",
         substr(x, start = nchar(x) - last + 1, stop = nchar(x)))
}

#' reimplementing purrr::possibly
maybe <- function (.f, otherwise = NULL, quiet = TRUE) {
  force(otherwise)
  function(...) {
    tryCatch(.f(...), error = function(e) {
      if (!quiet)
        message("Error: ", conditionMessage(e))
      otherwise
    })
  }
}
