mkrproj <- function(
    source            = Sys.getenv("MKRPROJ_SOURCE", "."),                      #"ask:URL to ZIP, ZIP file, or unzipped directory to use as template (can be "."):https://github.com/ecohealthalliance/mkrproj/archive/refs/heads/main.zip",
    project_name      = Sys.getenv("MKRPROJ_PROJECT_NAME", ""),           #"ask:Project/directory name? (no value for current directory):",
    parent_dir        = Sys.getenv("MKRPROJ_PARENT_DIR", "~/projects"),         #"ask:Parent directory for project? (no value for current directory):",
    #  use_git           = Sys.getenv("MKRPROJ_USE_GIT", "yes"),                       #"ask:Initiate git?:yes",
    #  use_github        = Sys.getenv("MKRPROJ_USE_GITHUB", "no"),                     #"ask:Initiate GitHub repository?:yes",
    #  github_org        = Sys.getenv("MKRPROJ_GITHUB_ORG", "ecohealthalliance"),  #"ask:What GitHub organization? (no value for personal account):",
    #  github_private    = Sys.getenv("MKRPROJ_GITHUB_PRIVATE", "yes"),            #"ask:Make GitHub repository private?:yes",
    #  use_gitcrypt      = Sys.getenv("MKRPROJ_USE_GITCRYPT", "yes"),                  #"ask:Initiate git-crypt encryption?:yes",
    #  gpg_user          = Sys.getenv("MKRPROJ_GPG_USER", ""),                     #"ask:What GPG user key should encrypt the repo (no value for `whoami::email_address()`):",
    #  use_github_secret = Sys.getenv("MKRPROJ_USE_GITHUB_SECRET", "yes"),             #"ask:Create a GitHub Actions repository secret to decrypt?:yes",
    #  install_shortcuts = Sys.getenv("MKRPROJ_INSTALL_SHORTCUTS", "no"),                  #"ask:Install default RStudio shortcuts?:yes",
    #  verbose           = Sys.getenv("MKRPROJ_VERBOSE", "no"),                 #"ask:Verbose output for debugging?:no",
    #  placeholder       = Sys.getenv("MKRPROJ_PLACEHOLDER", "_ehaproj-template_"),         #"ask:What is the placeholder in file names and files?:_mkrproj_",
    #  open_rstudio      = Sys.getenv("MKRPROJ_OPEN_RSTUDIO", "yes"),               #"ask:Open project in RStudio?:yes",
    #  use_coc           = Sys.getenv("MKRPROJ_COC", ""),                          #"ask:Contact for project Code of Conduct (no value for `whoami::email_address()`, 'no' for none):"
    #  remove            = Sys.getenv("MKRPROJ_REMOVE", "mkrproj.R;mkrproj.sh;.github/workflows/make-shellscript.yml;.github/workflows/update-packages.yml;")     #semicolon-separated files in the template that should be removed
) {




  #' Set variables based on env vars and interactive prompts
  collect_vars <- function(vars) {
    for (v in names(vars)) {
      vars[[v]] <- Sys.getenv(v, vars[[v]])
      if(grepl("^ask:[^:]+:.*$", vars[[v]])) {
        prompt <- gsub("^ask:([^:]+):.*$", "\\1", vars[[v]])
        default <- gsub("^ask:[^:]+:(.*)$", "\\1", vars[[v]])
        fullprompt <- paste0(prompt, " (default = '", default, "'): ")
        vars[[v]] <- read_input(fullprompt)
      }
      vars[[v]] <- response_to_logical(vars[[v]])

    }
    vars
  }

  # ---- Workflow Functions ----


  #' Replace filenames and text inside files with 'placeholder' with the project name


  bootstrap_packages <- function(verbose = TRUE) {
    if(verbose) message("• Installing packages...")
    renv::restore(prompt = FALSE)
    return(0)
  }

  run_pipeline <- function(verbose = TRUE) {
    if(!file.exists("_targets.R")) {
      message("No _targets.R in template, skipping pipeline.")
      return(1)
    }
    message("• Testing `targets` pipeline...")
    targets::tar_make(reporter = ifelse(verbose, "silent", "verbose"))

    return(0)
  }

  setup_git <- function(verbose = TRUE, dir = getwd()) {
    if(verbose) message("• Initiating git repository...")
    gert::git_init(dir)
    gert::git_add(".", repo = dir)
    gert::git_commit("Initial commit of project template", repo = dir)

    return(0)
  }


  setup_github <- function(project_name, github_org, private = TRUE, verbose = FALSE) {
    message("• Setting up GitHub repository...")
    if(github_org == "") github_org <- NULL
    if(gh::gh_token() == "") {
      message("  • No GITHUB_PAT env var found, GitHub repo will not be created.")
      return(FALSE)
    }
    if (is.null(github_org)) {
      create <- try(gh::gh("POST /user/repos", name = project_name,
                           private = private),
                    silent = TRUE)
    }
    else {
      create <- try(gh::gh("POST /orgs/{org}/repos", org = github_org,
                           private = private, name = project_name), silent = TRUE)
    }
    if(inherits(create, "try-error")) {
      message("  • Error communicating with GitHub, repo will not be created.")
      message(paste0("    ", strsplit(create, "\n")[[1]], collapse = "\n"))
      return(FALSE)
    }
    #origin_url <- switch(usethis::git_protocol(), https = create$clone_url,
    #                     ssh = create$ssh_url)
    #usethis::use_git_remote("origin", origin_url, overwrite = TRUE)
    gert::git_remote_add(url = create$clone_url, name = "origin")
    gert::git_push(remote = "origin", set_upstream = TRUE,
                   verbose = verbose)
    message("  • Repo created at ", create$html_url)
    return(create)
  }


  cleanup <- function(remove, dir = getwd(), git = TRUE, github = TRUE) {
    message("• Cleaning up packages and files...")
    unlink(file.path(dir, strsplit(remove, ";")[[1]]), recursive = TRUE)
    renv::clean(dir, actions = c("package.locks", "library.tempdirs", "system.library", "unused.packages"), prompt = FALSE)
    renv::snapshot(type = "implicit", prompt = FALSE)
    if (git) {
      gert::git_add(".")
      gert::git_commit("Customize and clean up template")
      if (github) {
        gert::git_push()
      }
    }

  }

  move_directory <- function(tmp_dir, parent_dir, project_name) {
    path <- file.path(parent_dir, project_name)
    message("• Moving project to ", path, "...")
    if(!dir.exists(parent_dir)) {
      dir.create(parent_dir, showWarnings = FALSE, recursive = TRUE)
    }
    fs::dir_copy(tmp_dir, path, overwrite = FALSE)
    return(0)
  }


  }


  # ---- Setup ----

  # Minimize impact on working environment
  wd <- getwd()
  libs <- .libPaths()
  op <- options()
  on.exit({
    .libPaths(libs)
    setwd(wd)
    options(op)
  })

  vars <- collect_vars(vars)
  list2env(
    stats::set_names(vars, gsub("MKRPROJ_", "", names(vars), fixed = TRUE)),
    environment())

  options(
    renv.config.startup.quiet = TRUE,
    renv.config.synchronized.check = FALSE,
    renv.verbose = FALSE)

  # ---- Workflow ----

  tmp_dir <- get_template(SOURCE)
  setwd(tmp_dir)

  rename_placeholders(project_name = PROJECT_NAME, placeholder = PLACEHOLDER)

  message("• Setting up renv...")

  suppressMessages({
    if(file.exists(".Rprofile")) {
      source(".Rprofile")
      options(renv.config.auto.snapshot = FALSE)
    } else if (file.exists("renv/activate.R")) {
      source("renv/activate.R")
    }
  })

  bootstrap_packages()
  run_pipeline(verbose = TRUE)
  if (GIT) setup_git()
  if (GITCRYPT) gitcrypt_success <- setup_crypt(gpg_user = GPG_USER)
  if (GITHUB) {
    github_repo <- setup_github(project_name = PROJECT_NAME,
                                github_org = GITHUB_ORG,
                                private = GITHUB_PRIVATE)
  }

  if (GITCRYPT && gitcrypt_success && GITHUB && inherits(github_repo, "gh_response")) {
    setup_secrets(repo = github_repo$name, owner = github_repo$owner$login)
  }

  if(!isFALSE(COC)) {
    contact <- ifelse(COC == "", whoami::email_address())
    message("• Adding a Code of Conduct with ", contact, " as contact.")
    suppressMessages({
      usethis::use_code_of_conduct(contact, ".github/")
    })
  }
  cleanup(remove = REMOVE, git = GIT, github = GITHUB)
  setwd(wd)
  move_directory(tmp_dir, PARENT_DIR, PROJECT_NAME)
  if(SHORTCUTS) add_rstudio_shortcuts()
  if(OPEN_RSTUDIO) open_project(parent_dir = PARENT_DIR, project_name = PROJECT_NAME)

}


#' Fetch the template into the target directory
#' @param source URL to ZIP, ZIP, or directory of template
#' @param dest the path to the directory to place the outputs
#' @param verbose
#' @return the path to the populated destination directory
get_template <- function(source, dest = tempfile(pattern = "dir"),
                         verbose = TRUE) {
  dir.create(temp_directory)

  if (grepl("^((http|ftp)s?|sftp)://", source)) {
    if (verbose) message("• Downloding template...\n")
    zipfile <- tempfile(fileext = ".zip")
    download.file(source, quiet = TRUE, destfile = zipfile)
    source <- zipfile
  }

  if (file.exists(source) && tools::file_ext(source) == "zip") {
    unzip(source, exdir = temp_directory)
    dirs <- list.dirs(temp_directory, full.names = TRUE, recursive = FALSE)
    if(length(dirs) == 1) file.rename(dirs, temp_directory)
  } else if (dir.exists(source)) {
    files <- list.files(source, all.files = TRUE, include.dirs = TRUE, full.names = TRUE)
    files <- files[!basename(files) %in% c(".git", ".Rproj.user", "_targets", ".Rhistory", ".DS_Store", "renv", ".", "..")]
    for (f in files) {
      file.copy(f, temp_directory, overwrite = TRUE, recursive = TRUE)
    }
    if(dir.exists(file.path(source, "renv"))) {
      dir.create(file.path(temp_directory, "renv"))
      file.copy(file.path(source, "renv", c("activate.R", ".gitignore", "settings.dcf")), file.path(temp_directory, "renv"))
    }
  } else {
    stop("Template not found or invalid. It must be a ZIP, directory, or URL of ZIP")
  }

  temp_directory
}

#' @param dir directory to initiate git-crypt on. Must be an active git repo.
#' @param gpg_user Who to initiate as an allowed user. If NULL, will use

#' Rename files and text in files
#' @param placeholder text to be replaced in filenames or in files with project
#'   name
#' @param template_marker text to be removed from filenames
#' @param dir directory to work in
#' @param verbose print messages
rename_placeholders <- function(dir = getwd(), project_name, placeholder = "_ehaproj_", template_marker = "-template", verbose = TRUE, dir = getwd()) {
  if (verbose) message("• Modifying template to use '", project_name, "' as name...")
  projfiles <- list.files(dir, all.files = TRUE, full.names = TRUE, recursive = TRUE)
  for (pfile in projfiles) {
    # Rename any files with the template name to the project name
    if(grepl(placeholder, pfile)) {
      new_name <- gsub(placeholder, project_name, pfile, fixed = TRUE)
      file.rename(pfile, new_name)
      pfile <- new_name
    }
    # Replace any text of placeholders with the project name
    lines <- readLines(pfile, warn = FALSE)
    updated_lines <- try(gsub(placeholder, project_name, lines, fixed = TRUE), silent = TRUE)
    cat(paste(updated_lines, collapse = "\n"), file = pfile)
  }

  templates <- list.files(dir, paste0(template_marker, "(\\.\\w+)?$"), all.files = TRUE, full.names = TRUE)
  file.rename(templates, gsub("-template" ,"", templates))

  return(TRUE)
}

#' Open the project directory in RStudio
#' @param path path to directory or .Rproj file
open_project <- function(path) {
  if(grepl("\\.Rproj$", path)) {
    rprojfile <- path
  } else {
    rprojfile <- list.files(path, pattern = "\\.Rproj$", full.names = TRUE)[1]
  }
  if(!length(rprojfile)) {
    stop("`path` must be an .Rproj file or a directory containing an .Rproj file.")
  }
  path <- file.path(parent_dir, project_name, paste0(project_name, ".Rproj"))
  if(Sys.getenv("RSTUDIO") == "1") {
    rstudioapi::openProject(path = path, newSession = FALSE)
  } else {
    os = get_os()
    switch(
      get_os(),
      osx = sys::exec_background("open", path),
      windows = sys::exec_background("start", path),
      linux = {
        xdgo <- Sys.which("xdg-open")
        if(xdgo == "") {
          message("  • xdg-open needed to open .Rproj files on Linux")
        } else {
          sys::exec_background("xdg-open", path)
        }
      })
  }
}

