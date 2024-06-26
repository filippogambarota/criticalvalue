% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/criticalvalue-funs.R
\name{critical_cor}
\alias{critical_cor}
\title{critical_cor}
\usage{
critical_cor(
  r = NULL,
  n,
  conf.level = 0.95,
  hypothesis = c("two.sided", "greater", "less"),
  test = c("t", "z")
)
}
\arguments{
\item{r}{a number corresponding to the correlation coefficient.}

\item{n}{a number corresponding to the sample size.}

\item{conf.level}{the confidence level to set the confidence interval, default is set to 0.95.}

\item{hypothesis}{a character string indicating the alternative hypothesis ("less", "greater" or "two.tailed").}

\item{test}{a parameter to specify which test to apply, either "t" for a t-test or "z" for a z-test.}
}
\value{
the output returns the critical correlation value, the degrees of freedom and the test used (either t or z).
}
\description{
This function allows to calculate the critical correlation value.
}
