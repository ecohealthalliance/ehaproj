#' Find and replace in files and filenames

#' Rename stuff
#'
#' @param dir directory in which to perform the operation
#' @param patterns a vector of fixed strings to find and replace
#' @param replacements a vector of replacements for each of the fixed strings
#' @param filenames filenames that should be modified
#' @param file a file to be modified
#' @rdname rename
rename_in_paths_and_files <- function(dir, patterns, replacements) {
  stopifnot(length(patterns) == length(replacements))
  ls <- dir_info(dir, recurse = TRUE, all = TRUE)
  files <- ls$path[ls$type == "file"]

  rename_in_paths(ls$path, patterns, replacements)
  walk(files, \(f) rename_in_file(f, patterns, replacements))

}

#' @export
#' @rdname rename
rename_in_filenames <- function(rename_in_filenames, patterns, replacements) {
  newrename_in_filenames <- stringi::stri_replace_all_fixed(rename_in_filenames, patterns, replacements, vectorise_all = FALSE)
  changed <- paths != rename_in_filenames
  file_move(path[changed], rename_in_filenames[changed])
}

#' @export
#' @rdname rename
rename_in_file <- function(file, patterns, replacements) {
  contents <- paste(readLines(file), collapse = "\n")
  updated_contents <- stringi::stri_replace_all_fixed(contents, patterns, replacements, vectorise_all = FALSE)
  if(contents != updated_contents) {
    cat(update_contents, file=file, sep="", append = FALSE)
  }
  return(file)
}
