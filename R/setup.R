#' Create a new project
#' @export
#' @param ... project configuration options.  See `ehaproj_config()` for all
#'   options
#' @param .options configuration options passed as a list
ehaproj <- function(..., .options = list()) {

  options <- ehaproj_config(..., .options = .options)

  vb(bullet("Collecting information about your system..."))
  sitrep_user <- get_sitrep_user()

  options <- reconcile_options(options, sitrep)


  vb(bullet("Fetching the template..."))
  tempdir <- get_ehaproj_template()
  vb(bullet("Customizing the template..."))
  renames <- c(
    "{{projectname}}" = project_options$projectname,
    "{{projectdesc}}" = project_options$projectdesc,
    "{{projectrepo}}" = paste0(project_options$github_ns, project_options$projectname, sep = "/"),
    "{{template_sha}}" = attr(tempdir, "sha")
  )
  rename_in_paths_and_files(tempdir,
      patterns = names(renames),
      replacements = unname(renames))

  if(project_options$use_git) {
    gert::git_init(tempdir)
    gert::git_commit_all(tempdir, message = "Initial commit")
  }

  if(project_options$use_github) {

  }

  if(project_options$use_gitcrypt) {

  }

  vb(bullet("Finalizing..."))
  file_move(tempdir, project_options$project_dir)

}


fn <- function(..., .opts = NULL) {
  options <- c(list(...), .opts)
  options
}




