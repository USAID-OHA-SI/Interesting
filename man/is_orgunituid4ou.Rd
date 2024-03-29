% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cir_dqa_checks.R
\name{is_orgunituid4ou}
\alias{is_orgunituid4ou}
\title{Check if orgunituid exist in operating unit}
\usage{
is_orgunituid4ou(.data, df_orgs)
}
\arguments{
\item{df_orgs}{org_hierarchy df}

\item{data}{df}
}
\value{
data with new column: T/F
}
\description{
Check if orgunituid exist in operating unit
}
\examples{
\dontrun{
  data \%>\% orgunituid4ou(df_orgs)
}

}
