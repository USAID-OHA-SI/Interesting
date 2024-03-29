% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cir_dqa_checks.R
\name{is_mech4ou}
\alias{is_mech4ou}
\title{Validate mechanism code}
\usage{
is_mech4ou(.data, df_mechs)
}
\arguments{
\item{.data}{df}

\item{df_mechs}{mechs}
}
\value{
boolean
}
\description{
Validate mechanism code
}
\examples{
\dontrun{
  data \%>\% is_mech4ou_valid(df_mechs)
}
}
