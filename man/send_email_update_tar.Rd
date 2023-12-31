% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-send-email.R
\name{send_email_update_tar}
\alias{send_email_update_tar}
\title{Send email alerts to updated automation reports}
\usage{
send_email_update_tar(
  to,
  from = "rsconnect@ecohealthalliance.org",
  project_name,
  use_hyperlinks = FALSE,
  hyperlinks_text = NULL,
  attach = FALSE,
  test = FALSE,
  target,
  pattern = "\\\\.html"
)
}
\arguments{
\item{to}{A vector of email addresses serving as primary recipients for the
message.}

\item{from}{The email address of the sender.}

\item{project_name}{String. Name of the project to use in email subject and body text.}

\item{use_hyperlinks}{Logical. If TRUE, a hyperlink using the file name or custom text is provided instead of the
the full url of the report.}

\item{hyperlinks_text}{String.  NULL, hyperlink will be Current_ReportBasename
eg Current_MyMarkdownReport.html. If a string or vector of strings are provided
those will be used for the text in the hyperlink.}

\item{attach}{Logical. Should reports be attached to email? Default is
FALSE. If TRUE, all HTML reports found in \emph{"outputs"} folder will be
attached to email.}

\item{test}{Logical. Is this an email alert for testing automation reports?
Default is FALSE. If TRUE, subject includes test and upload path will include
the current git branch in the file path (e.g. https://project.secure.eha.io/refs/fix/missing_documentation/file.txt)}

\item{target}{Targets object. List of file paths.}

\item{pattern}{String. Regex pattern to select specific files in path.}
}
\value{
Invisible. Update email sent to list of recipients in \code{to}
}
\description{
Send email alerts to updated automation reports
}
\seealso{
\code{browseVignettes("blastula")}
}
