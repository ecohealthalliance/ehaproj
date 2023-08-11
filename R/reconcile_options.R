#' Reconcile options with each other and the user environment
#'
#' @param options a list of ehaproj configuration options
#' @param sitrep a list produced by [get_sitrep_user()].
#' @author 'Noam Ross'
#' @export
reconcile_options <- function(options, user_sitrep = get_sitrep_user()) {

}

process_options <- function(options = ehaproj_config()) {
  info <- ehaproj_config_info()

  options <- map(options)
}
