#'
#' @export
gpg_installed <- function() {
  info <- gpg::gpg_info()
  val <- sys::exec_wait(info$gpg, "--version", std_out = FALSE) == 0
  if (val) attr(val, "version") <- info$version

  val
}

gpg_bin <- function() {
  gpg::gpg_info()$gpg
}

#' @importFrom stringi stri_extract_all_regex
gpg_list_emails <- function(private = FALSE) {
  flag = if(private) "--list-secret-keys" else "--list-keys"
  stopifnot(gpg_installed())
  txt <- rawToChar(sys::exec_internal(gpg_bin(), flag)$stdout)
  emails <- sort(unique(na.omit(unlist(stringi::stri_extract_all_regex(txt, "[\\w\\.]+@[\\w\\.]+")))))
  emails
}
