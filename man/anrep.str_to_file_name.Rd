% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_functions.R
\name{anrep.str_to_file_name}
\alias{anrep.str_to_file_name}
\title{Convert a string to a (more or less) valid file name}
\usage{
anrep.str_to_file_name(x, max.length = 0)
}
\arguments{
\item{x}{String}

\item{max.length}{Limit to the length of the returned string}
}
\description{
Convert a string to a (more or less) valid file name
}
\examples{
anrep.str_to_file_name("My long $ and ! || && strange string")
}
