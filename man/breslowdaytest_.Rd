% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BreslowDayFunction.R
\name{breslowdaytest_}
\alias{breslowdaytest_}
\title{Breslow-Day test}
\usage{
breslowdaytest_(x, odds_ratio = NA, correct = FALSE)
}
\arguments{
\item{x}{a 2x2xK contingency table}

\item{odds_ratio}{Odds Ration (default = NA)}

\item{correct}{if TRUE Tarones correction is returned. Default = FALSE.}
}
\value{
A vector with three values statistic - Breslow and Day test
statistic pval - p value evtl. based on the Tarone test statistic using a
\eqn{\chi^2(K-1)} distribution
}
\description{
Breslow-Day test
}
