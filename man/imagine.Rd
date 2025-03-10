% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imagine.R
\name{imagine}
\alias{imagine}
\title{Combine draw, getweights, and boil}
\usage{
imagine(z, m, x, f, y, B, w = 1, g = stats::weighted.mean, ...)
}
\arguments{
\item{z}{a (conceptual) row of a data.frame given by \code{pick}.}

\item{m}{a model object.}

\item{x}{matrix. a data set; a population.}

\item{f}{function. a boiler.}

\item{y}{numeric. values of the dependent variable representing success under \code{f}.}

\item{B}{matrix. A set of plausible coefficient vectors generated via \code{\link{consider}}.}

\item{w}{numeric. weights with respect to which to take the \code{g} function. Default \code{1}.}

\item{g}{function. the desired one-number summary of the boiled values. Default \code{stats::\link[stats]{weighted.mean}}.}

\item{...}{other parameters ignored for the moment, but to be implemented.}
}
\value{
a \code{data.frame} of simulated outcome summary statistics.
}
\description{
This is exported, but don't call it directly.
}
