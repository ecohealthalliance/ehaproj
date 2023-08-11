#' @export
#' @importFrom desc desc
pkg_version <- function(pkg) {
  d <- desc::desc(file = system.file("DESCRIPTION", package=pkg))
  ec(
    list(version = d$get_version(),
       sha = maybe(d$get_field, otherwise = character(0))("RemoteSha")),
       "ehaproj_pkg_version")
}

#' @export
format.ehaproj_pkg_version <- function(x) {
  glue_collapse(c(as.character(x$version), substr(x$sha, 1, 8)), sep = ", ")
}
