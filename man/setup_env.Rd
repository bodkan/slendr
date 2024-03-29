% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interface.R
\name{setup_env}
\alias{setup_env}
\title{Setup a dedicated Python virtual environment for slendr}
\usage{
setup_env(quiet = FALSE, agree = FALSE, pip = FALSE)
}
\arguments{
\item{quiet}{Should informative messages be printed to the console? Default
is \code{FALSE}.}

\item{agree}{Automatically agree to all questions?}

\item{pip}{Should pip be used instead of conda for installing slendr's Python
dependencies?}
}
\value{
No return value, called for side effects
}
\description{
This function will automatically download a Python miniconda distribution
dedicated to an R-Python interface. It will also create a slendr-specific
Python environment with all the required Python dependencies.
}
