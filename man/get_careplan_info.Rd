% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{get_careplan_info}
\alias{get_careplan_info}
\title{Get careplan info}
\usage{
get_careplan_info(
  patient_id,
  access_token,
  base_careplan_url,
  language = "nl",
  check_ssl = TRUE,
  debug = FALSE,
  careplan_filter = NULL
)
}
\arguments{
\item{patient_id}{String formatted like "patient_id@organisation_id".}

\item{access_token}{API access token.}

\item{base_careplan_url}{API endpoint for careplan data.}

\item{language}{Language of the response data.}

\item{check_ssl}{Whether to check the SSL certificate or allow insecure
connections.}

\item{debug}{Whether to enable debugging messages.}

\item{careplan_filter}{Vector with careplan names to filter on.}
}
\value{
Data.table containing careplan data.
}
\description{
Get careplan info
}
\examples{
\dontrun{
get_careplan_info("555555@70", access_token = "<token>",
  base_careplan_url = "<url>")
}

}
