#'   whoami::email_address(), which defaults to the primary git user.
use_gitcrypt <- function(dir = getwd(), gpg_user = NULL, verbose = TRUE) {

  if (!gitcrypt_installed()) cli_abort("git-crypt not found")

  if(is.null(gpg_user) || gpg_user == "") gpg_user <- whoami::email_address()
  gpg_id <- gpg::gpg_list_keys(search = gsub("('|\\\")", "", gpg_user))

  sys::exec_wait("git-crypt", "init", std_out = FALSE, std_err = FALSE)
  if(verbose) message("  • Adding ", gpg_id$email, "'s user key...")
  sys::exec_wait("git-crypt", c("add-gpg-user", gpg_id$fingerprint), std_out = FALSE)

  if(verbose) {
    encrypted <- character(0)
    if(file.exists(".gitattributes")) {
      gitattr <- stringi::stri_subset_fixed(
        readLines(".gitattributes", warn = FALSE),
        "filter=git-crypt diff=git-crypt")
      encrypted <- stringi::stri_extract_first_regex(gitattr, "^[^\\s]+")
    }
    if(length(encrypted)) {
      message("  • Paths to be encrypted: ", paste(encrypted, collapse = ", "))
    } else {
      message("  • No paths set to be encrypted, modify `.gitattributes` to do so (see  <https://www.agwa.name/projects/git-crypt/>)")
    }
  }

  return(TRUE)
}

#' @importFrom processx run
gitcrypt <- function(args = character(), dir = ".", ...) {
  run(gitcrypt_bin(), args, wd = dir, ...)
}

gitcrypt_bin <- function() {
  Sys.getenv("GITCRYPT_PATH", Sys.which("git-crypt"))
}

gitcrypt_installed <- function() {
  if (gitcrypt_bin() == "") return(FALSE)
  res <- gitcrypt("version", error_on_status = FALSE)
  val <- res$status == 0
  if(val) attr(val, "version") <- stringi::stri_trim(res$stdout)
  val
}

use_gitcrypt_gh_secret <- function(repo, owner, secret_name = "GIT_CRYPT_KEY64", dir = ".") {
  if (!gitcrypt_installed()) cli_abort("git-crypt not found")
  tkey <- tempfile()
  on.exit(if (file_exists(tkey)) file_delete(tkey))

  res <- gitcrypt(c("export-key", tkey), dir = dir)
  if (res$status != 0) cli_abort(paste0("git-crypt key export errored with status ", res$status, ". stderr: ", res$status))

  key64 <- base64enc::base64encode(tkey)
  gh_set_repo_secret(owner, repo, secret = key64, secret_name = secret_name)
}

#' @export
#' @rdname gitcrypt
#' @param dir the directory to operate on
gitcrypt_unlock <- function(dir = ".") {
  gitcrypt("unlock", dir = dir)
}

#' @export
#' @param force lock even though the repository is not clean
#' @param all lock all keys, not just default
#' @param key lock a specific key
#' @rdname gitcrypt
gitcrypt_lock <- function(dir = ".", force = FALSE, all = FALSE, key = NULL) {
  args <- c("lock")
  if(force) args <- c(args, "--force")
  if(all) args <- c(args, "--all")
  if (!is.null(key)) ars <- c(args, "--key-name", key)
  gitcrypt(args, dir = dir)
}


#' Checks if a file is encrypted with git-crypt.
#'
#' Failing/forgetting to run "`git-crypt unlock` when checking out an encrypted
#' repository is a common source of errors.  Such files start with the header
#' `^@GITCRYPT^@` (where `^@` are null binary bytes), which this function
#' checks for.
#'
#' @param f a file to check
#' @rdname gitcrypt
is_file_gitencrypted <- function(f) {
  con <- file(f, "rb")
  on.exit(close.connection(con))
  x <- readBin(file(f, "rb"), what = "raw", n = 10)
  gitencrypted <- identical(x, as.raw(c(0x00, 0x47, 0x49, 0x54, 0x43, 0x52, 0x59, 0x50, 0x54, 0x00)))

  gitencrypted
}

read_gitattributes <- function(f = ".gitattributes") {
  l <- readLines(f)
  l <- stringi::stri_subset_regex(l, "(^#|^\\s*$)", negate = TRUE) # Remove comments and empty lines
  l <- stringi::stri_split_regex(l, "\\s+")
  ec(structure(lapply(l, \(x) x[-1]), .Names=vapply(l, \(x) x[1], character(1))), "gitattributes")
}


