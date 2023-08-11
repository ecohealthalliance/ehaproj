get_ehaproj_template_remote <- function(repo = "ecohealthalliance/container-template", ref = "main", dest = file_temp("dir")) {
  template_sha <- gh::gh(glue("GET /repos/{repo}/commits/{ref}"))$sha
  zipfile <- gh::gh(glue("GET /repos/{repo}/zipball/{ref}"), .destfile = file_temp(ext = ".zip"))
  dir <- path_common(unzip(zipfile, exdir = tempdir()))
  file_move(dir, dest)
  attr(dest, "sha") <- template_sha
  dest
}

get_ehaproj_template_local <- function(dest = file_temp("dir")) {
  dir_copy(system.file("ehaproj-template", package = "ehaproj"), dest)
  attr(dest, "sha") <- readLines(system.file("TEMPLATE_SHA", package = "ehaproj"))
  dest
}

#' @export
get_ehaproj_template <- function(path = file_temp("dir"), remote = ehaproj_has_internet(), ...) {
  dir <- NULL

  if (remote) {
    dir <- maybe(get_ehaproj_template_remote)(dest = dest, ...)
    vb(cli_warn("Could not get template from GitHub, using local copy"))
  }
  if(is.null(dir)) {
    dir <- get_ehaproj_template_local(dest = dest)
  }
  gitfile <- path(dir, ".git")
  if(file_exists(gitfile)) file_delete(gitfile)

}
