# # Drawn from the `renv` source code
# ehaproj_roxygen_config_section <- function() {
#
#   # read config
#   config <- yaml::read_yaml("inst/config.yml")
#
#   # generate items
#   items <- map_chr(config, function(entry) {
#
#     # extract fields
#     name <- entry$name
#     type <- entry$type
#     default <- entry$default
#     description <- entry$description
#
#     # deparse default value
#     default <- case(
#       identical(default, list()) ~ "NULL",
#       TRUE                       ~ deparse(default)
#     )
#
#     # generate table row
#     fmt <- "\\subsection{renv.config.%s}{%s Defaults to \\code{%s}.}"
#     sprintf(fmt, name, description, default)
#
#   })
#
#   c(
#     "@section Configuration:",
#     "",
#     "The following renv configuration options are available:",
#     "",
#     items,
#     ""
#   )
#
# }
