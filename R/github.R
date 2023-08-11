#' List the organizations a user is allowed to create repositories and projects in.
#'
#' Used to provide a selection in setting up a project
#' @param username The user's GitHub username.
#' @importFrom gh gh
#' @importFrom dplyr bind_rows mutate filter case_when
#' @importFrom tibble tibble
#' @return A tibble of the organizations where the user has create repository permissions, printed as a single line
#' @export
gh_can_create_in_orgs <- function() {
  res <- gql("{ viewer { organizations(first: 100) { edges {
        node {
          viewerCanCreateRepositories
          viewerCanCreateProjects
          login
        } } } } }")

  repos =  Filter(\(x) x$node$viewerCanCreateRepositories,
         res$data$viewer$organizations$edges) |>
    vapply(\(x) x$node$login, character(1))
  projects = Filter(\(x) x$node$viewerCanCreateRepositories,
                    res$data$viewer$organizations$edges) |>
    vapply(\(x) x$node$login, character(1))

  list(repos = repos, projects = projects)
}

gh_status <- function(url = "https://www.githubstatus.com/api/v2/status.json") {
  jsonlite::read_json(url)
}

gh_set_repo_secret <- function(owner, repo, secret, secret_name) {
  repo_public_key <- gh::gh("/repos/{owner}/{repo}/actions/secrets/public-key", repo = repo, owner = owner)
  encrypted_secret <- sodium::simple_encrypt(charToRaw(secret), base64enc::base64decode(public_key$key))
  gh::gh("PUT /repos/{owner}/{repo}/actions/secrets/{secret_name}",
         owner = owner, repo = repo, secret_name = secret_name,
         encrypted_value = encrypted_secret, key_id = public_key$key_id)
}

#' Get a list of repositories under a GitHub namespace,
#'
#' Used to test if a project name already exists
#' @param org The GitHub organization. If NULL, defaults to the user's space
#' @export
gh_list_repos <- function(org = NULL) {
  cursor <- 'null'
  has_next_page <- TRUE
  query <- '{ <<qstart>> {
        repositories(first: 100, after: <<cursor>>, affiliations: [OWNER]) {
          totalCount
          pageInfo {
            endCursor
            hasNextPage
          }
          nodes {
            name
          }
        }}}'

  if (is.null(org)) {
    field <- "viewer"
    qstart <- "viewer"
  } else {
    field <- "organization"
    qstart <- glue('organization(login: "{org}")')
  }

  repos <- list()
  while (has_next_page) {
    res <- gql(glue(query, .open = "<<", .close = ">>"))
    repos <- c(repos, res$data[[field]]$repositories$nodes)
    has_next_page <- res$data[[field]]$repositories$pageInfo$hasNextPage
    cursor <- paste0('"', res$data[[field]]$repositories$pageInfo$endCursor, '"')
  }
  repos <- vapply(repos, \(x) x$name, character(1))

  repos
}

format.gh_pat <- function(x, ...) {
  if (x == "") {
    "<no PAT>"
  }
  else {
    obfuscate(x)
  }
}
