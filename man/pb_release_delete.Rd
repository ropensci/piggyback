% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pb_release_delete.R
\name{pb_release_delete}
\alias{pb_release_delete}
\title{Delete release from GitHub repo}
\usage{
pb_release_delete(repo = guess_repo(), tag, .token = gh::gh_token())
}
\arguments{
\item{repo}{Repository name in format "owner/repo". Defaults to \code{guess_repo()}.}

\item{tag}{tag name to delete. Must be one of those found in \code{pb_releases()$tag_name}.}

\item{.token}{GitHub authentication token, see \verb{[gh::gh_token()]}}
}
\description{
Delete release from GitHub repo
}
\examples{
\dontrun{
pb_release_delete("cboettig/piggyback-tests", "v0.0.5")
}
}
\seealso{
Other release_management: 
\code{\link{pb_release_create}()}
}
\concept{release_management}
