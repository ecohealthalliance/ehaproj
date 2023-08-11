#' Situation Reports
#'
#' These functions check the staus of machine configuration of or project
#' setup.
#'
#' @return A list of statuses, with NA values being indeterminate.  The list
#' is of class `ehaproj_status` and has a pretty-printing method.
#' @examples
#'   sitrep()
#' @rdname sitrep
#' @export
get_sitrep <- function() {
  list(
    sitrep_user = get_sitrep_user(),
    sitrep_project = get_sitrep_project()
  )
}

#' @rdname sitrep
#' @export
#' @importFrom purrr possibly
#' @importFrom dplyr if_else
get_sitrep_user <- function() {

  r_version = R.Version()
  sys_info = Sys.info()
  ehaproj_version <- pkg_version("ehaproj")

  git_installed <- git_installed()
  gitconfig <- maybe(gert::git_config_global)()

  github_personal_access_token <- gh::gh_token()

  aws_credentials_file_exists <- file_exists(aws_creds_path())
  aws_profiles <- if (aws_credentials_file_exists) maybe(\() glue_collapse(names(aws_creds()), ", "))() else NULL

  has_internet <- ehaproj_has_internet()

  if (has_internet)  {
    github_status <- gh_status()$status$description
    if(github_status != "All Systems Operational") attr(github_status, "status") <- "warning"
  } else {
    github_status <- NULL
    aws_permissions <- NULL
  }

  aws_permissions <- if (has_internet && !is.null(aws_profiles)) aws_get_permissions() else NULL
  aws_has_permissions <- if(!is.null(aws_permissions)) aws_check_permissions(aws_permissions) else NULL

  gh_who <- if (has_internet & github_personal_access_token != "") maybe(gh::gh_whoami)() else NULL
  gh_orgs <- if(!is.null(gh_who)) gh_can_create_repos_in() else NULL


  # Add "sitrep_heading" to create printing breakpoints
  status <- tibble::lst(
    "Environment" = "sitrep_heading",
    r_version = r_version$version.string,
    os = paste(sys_info["sysname"], sys_info["release"]),
    system_user = unname(sys_info["user"]),
    ehaproj_version = ehaproj_version,

    "Git" = "sitrep_heading",
    git_installed = git_installed,
    git_username = gitconfig$value[gitconfig$name == "user.name"],
    git_email = gitconfig$value[gitconfig$name == "user.email"],

    "GitHub" = "sitrep_heading",
    has_internet = has_internet,
    github_status = github_status,
    github_personal_access_token = github_personal_access_token,
    github_username = gh_who$login,
    github_token_scopes = gh_who$scopes,
    github_can_create_repos_in = gh_orgs,

    "Encryption" = "sitrep_heading",
    gpg_installed = gpg_installed(),
    gpg_public_key_emails = if_else(gpg_installed, paste(gpg_list_emails(), collapse = ", "),  NA_character_),
    gpg_private_key_emails = if_else(gpg_installed, paste(gpg_list_emails(private = TRUE), collapse = ", "),  NA_character_),
    git_crypt_installed = gitcrypt_installed(),

    "Amazon Web Services" = "sitrep_heading",
    aws_credentials_file_exists = aws_credentials_file_exists,
    aws_profiles = aws_profiles,
    aws_user = aws_permissions$user,
    aws_account = aws_permissions$account,
    aws_permissions = aws_permissions$permissions,
    aws_has_permissions = aws_has_permissions
  )
  headings <- which(purrr::map_lgl(status, \(x) !is.null(x) && !is.list(x) && x == "sitrep_heading"))
  section_names <- names(status)[headings]
  section_starts <- names(status)[headings + 1]
  status <- status[-headings]
  status <- ec(status, "ehaproj_sitrep")
  attr(status, "section_starts") <- set_names(section_starts, section_names)

  status

}


#' @rdname sitrep
#' @export
get_sitrep_project <- function() {
  list()
}


#' @importFrom purrr iwalk
#' @export
format.ehaproj_sitrep <- function(status) {
  status_attr <- function(x) attr(x, "status") %||% ""
  section_starts <- attr(status, "section_starts")
  section_names <- names(section_starts)

  cli_fmt({
    cli_h2("User/Machine Status")
    iwalk(status, function(value, name) {
      valtxt <- glue_collapse(c(as.character(format(value)), as.character(attr(value, "version"))), sep = ", ")
      text <- paste0(name, ": ", as.character(valtxt))
      if (name %in% section_starts) cli_text(style_bold(section_names[section_starts == name]))
      if(!is.list(value) && (is_empty(value) || is_na(value) || value == "" || !length(value) ||  status_attr(value) == "empty")) {
        cli_alert_warning(text)
      } else if(isFALSE(value) || status_attr(value) == "warning") {
        cli_alert_danger(text)
      } else {
        cli_alert_success(text)
      }
    })
  })
}

ehaproj_has_internet <- function() {
  curl::has_internet() && !(Sys.getenv("EHAPROJ_NO_INTERNET") %in% c("TRUE", "true", "1"))
}
