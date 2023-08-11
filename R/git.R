#' @importFrom processx ruun
git_installed <- function() {
  gitpath <- Sys.which("git")
  run(gitpath, "--version", error_on_status = FALSE)
  val <- res$status == 0
  if (val) attr(val, "version") <- stringi::stri_trim(res$stdout)

  val
}
