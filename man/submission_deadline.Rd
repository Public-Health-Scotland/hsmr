% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/submission_deadline.R
\name{submission_deadline}
\alias{submission_deadline}
\title{SMR01 submission deadline}
\usage{
submission_deadline(end_date)
}
\arguments{
\item{end_date}{The cut-off date for SMR01 data to be included in the
current HSMR publication, supplied with \code{Date} class. Must be the final
day of either March, June, September or December.}
}
\description{
\code{submission_deadline} takes the final date for which data
from the General/Acute Inpatient and Day Case (SMR01) dataset are included
in the current publication. It returns the date on which the completeness of
SMR01 submissions to ISD for individual hospitals is reported in the current
publication.
}
\details{
The submission deadline is always the final day of the month in
which the previous publication was published, plus 42 days.
}
\examples{
extract_end_date <- lubridate::dmy(30092018)

submission_deadline(end_date = extract_end_date)
format(submission_deadline(end_date = extract_end_date), "\%d \%B \%Y")
format(submission_deadline(end_date = extract_end_date), "\%d/\%m/\%Y")

}
