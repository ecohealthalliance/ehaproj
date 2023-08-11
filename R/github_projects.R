#' Create a GitHub Project Board
#'
#' This creates a GitHub project.  Defaults are to use EHA projects templates and automations
#' @export
#' @param template a GitHub Project to use as a template. Can take name, number or GraphQL ID of a project, preceeded with "organiation/"
#' @param repo A GitHub repository to associate the project with
#' @param organization or user namespace. NULL uses the current user
gh_create_project <- function(organization = NULL, template = NULL, repo = NULL) {


  # TODO: query to lookup projects to copy from template, actions to set up
  # and activate default automations

  # Get template
  # List projects
  gh::gh("POST /graphql", query = '{
  viewer {
    login
    organization(login: "ecohealthalliance") {
    projectsV2(first:5) {
      edges {
      node {
      id
      title
      }
      }
    }
    }
  }
}',
         .send_headers = c('X-Github-Next-Global-ID'='1'))


  gh::gh("POST /graphql", query = '
             mutation MyMutation {
  copyProjectV2(input: {projectId: "PVT_kwDOABboL84ATMxo", ownerId: "O_kgDOABboLw", title: "TEST6"}) {
    clientMutationId
    projectV2 { id }
  }
}
',
.send_headers = c('X-Github-Next-Global-ID'='1'))

  gh::gh()



  # Check out https://docs.github.com/en/graphql/overview/explorer to find endpoints
  # https://docs.github.com/en/graphql/reference/mutations#copyprojectv2


  # Create a project
  gh::gh_gql('
  "mutation (`$ownerId: ID!, `$title: String!) { createProjectV2(input: { ownerId: `$ownerId, title: `$title }) { projectV2 { id } } }"
  ')


  # Create a project
  gh::gh_gql('
  "mutation (`$ownerId: ID!, `$title: String!) { createProjectV2(input: { ownerId: `$ownerId, title: `$title }) { projectV2 { id } } }"
  ')
}


#' Same as gh::gh_gql, but use updated Global IDs in header by default
gql <- function(query, ...) {
  gh(endpoint = "POST /graphql", query = query,
     .send_headers = c('X-Github-Next-Global-ID'='1'),
     ...)
}
