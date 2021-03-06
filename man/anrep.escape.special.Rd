% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_functions.R
\name{anrep.escape.special}
\alias{anrep.escape.special}
\title{Quote all Markdown special symbols in a string}
\usage{
anrep.escape.special(x)
}
\arguments{
\item{x}{Input string}
}
\description{
Use this protect Markdown formatting symbols from being interpreted
when Markdown is rendered
}
\examples{
anrep.escape.special("_a_")
anrep.escape.special("**a**[bbb](ccc)")
anrep.escape.special("`a`")
}
