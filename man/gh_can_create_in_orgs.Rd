% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gh.R
\name{gh_can_create_in_orgs}
\alias{gh_can_create_in_orgs}
\title{List the organizations a user is allowed to create repositories and projects in.}
\usage{
gh_can_create_in_orgs()
}
\arguments{
\item{username}{The user's GitHub username.}
}
\value{
A tibble of the organizations where the user has create repository permissions, printed as a single line
}
\description{
Used to provide a selection in setting up a project
}
