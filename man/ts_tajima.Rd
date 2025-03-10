% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree-sequences.R
\name{ts_tajima}
\alias{ts_tajima}
\title{Calculate Tajima's D for given sets of individuals}
\usage{
ts_tajima(ts, sample_sets, mode = c("site", "branch", "node"), windows = NULL)
}
\arguments{
\item{ts}{Tree sequence object of the class \code{slendr_ts}}

\item{sample_sets}{A list (optionally a named list) of character vectors with
individual names (one vector per set). If a simple vector is provided, it
will be interpreted as \code{as.list(sample_sets)}, meaning that a given
statistic will be calculated for each individual separately.}

\item{mode}{The mode for the calculation ("sites" or "branch")}

\item{windows}{Coordinates of breakpoints between windows. The first
coordinate (0) and the last coordinate (equal to \code{ts$sequence_length})
are added automatically)}
}
\value{
For each set of individuals either a single Tajima's D value or a
vector of Tajima's D values (one for each window)
}
\description{
For a discussion on the difference between "site" and "branch" options of the
\code{mode} argument, please see the tskit documentation at
\url{https://tskit.dev/tskit/docs/stable/stats.html#sec-stats-mode}
}
\examples{
\dontshow{check_dependencies(python = TRUE, quit = TRUE) # dependencies must be present
}
init_env()

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk
ts <- ts_read(slendr_ts, model) \%>\% ts_mutate(mutation_rate = 1e-8, random_seed = 42)

# calculate Tajima's D for given sets of individuals in a tree sequence ts
ts_tajima(ts, list(eur = c("EUR_1", "EUR_2", "EUR_3", "EUR_4", "EUR_5"),
                   nea = c("NEA_1", "NEA_2")))
}
