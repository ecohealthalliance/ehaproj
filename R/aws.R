#' Edit your AWS credentials
#' @export
#' @importFrom usethis edit_file
#' @rdname aws_creds
edit_aws_creds <- function() {
  edit_file(aws_creds_path())
}

#' @export
#' @rdname aws_creds
#' @importFrom ini read.ini
aws_creds <- function(file = aws_creds_path()) {
  read.ini(file)
}

#' @export
#' @rdname aws_creds
aws_creds_path <- function() {

  if (.Platform$OS.type == "unix") {
    home <- "~"
  } else {
    home <- Sys.getenv("USERPROFILE")
  }

  path <- Sys.getenv("AWS_SHARED_CREDENTIALS_FILE",
                     unset = path.expand(file.path(home, ".aws/credentials")))

  path
}

#' Lookup user AWS information and permissions
#' @param username The AWS account username, if NULL, defaults to that
#'   associated with current credentials
#' @param actions A list of permissions to check for
#' @param credentials A list with members `aws_access_key_id` and
#'   `aws_secret_access_key`. Defaults to lookup up the user `.aws/credentials`
#'   rather than environment variables.
#' @export
aws_get_permissions <- function(
    username = NULL,
    actions = list("s3:CreateBucket", "s3:ListBuckets", "iam:CreateUser", "iam:CreateAccessKey"),
    credentials = aws_creds()$default) {

  iam_svc <- paws::iam(config = list(
    credentials = list(
      creds = list(
        access_key_id = credentials$aws_access_key_id,
        secret_access_key = credentials$aws_secret_access_key
      ))))

  user <- iam_svc$get_user(UserName = username)
  account <- iam_svc$list_account_aliases()$AccountAliases
  permissions <- iam_svc$simulate_principal_policy(
    user$User$Arn, ActionNames = actions)
  permissions <- ec(permissions, "aws_permissions")


  aws_permissions <- list(
    user = user$User$UserName,
    account = account,
    permissions = permissions
  )

  aws_permissions
}

aws_check_permissions <- function(permissions, required = c("s3:CreateBucket", "s3:ListBuckets")) {
  allowed <-
    map_chr(permissions$permissions$EvaluationResults, \(x) x[["EvalActionName"]])[
      map_lgl(permissions$permissions$EvaluationResults, \(x) x[["EvalResourceName"]] == "*" && x[["EvalDecision"]] == "allowed")
    ]
  all(required %in% allowed)
}

#' @export
format.aws_permissions <- function(x) {
  if(is.null(x)) return(NULL)
  permissions = glue_collapse(vapply(x$EvaluationResults, function(res) {
    glue("{res$EvalActionName} ({res$EvalResourceName}, {res$EvalDecision})")
  }, character(1)), sep = ", ")
}

