% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree-sequences.R
\name{ts_afs}
\alias{ts_afs}
\title{Compute the allele frequency spectrum (AFS)}
\usage{
ts_afs(
  ts,
  sample_sets = NULL,
  mode = c("site", "branch", "node"),
  windows = NULL,
  span_normalise = FALSE,
  polarised = TRUE
)
}
\arguments{
\item{ts}{Tree sequence object of the class \code{slendr_ts}}

\item{sample_sets}{A list (optionally a named list) of character vectors with
individual names (one vector per set). If NULL, allele frequency spectrum
for all individuals in the tree sequence will be computed.}

\item{mode}{The mode for the calculation ("sites" or "branch")}

\item{windows}{Coordinates of breakpoints between windows. The first
coordinate (0) and the last coordinate (equal to \code{ts$sequence_length})
are added automatically)}

\item{span_normalise}{Argument passed to tskit's \code{allele_frequency_spectrum}
method}

\item{polarised}{When TRUE (the default) the allele frequency spectrum will
not be folded (i.e. the counts will assume knowledge of which allele is ancestral,
and which is derived, which is known in a simulation)}
}
\value{
Allele frequency spectrum values for the given sample set. Note that the
contents of the first and last elements of the AFS might surprise you. Read the
links in the description for more detail on how tskit handles things.
}
\description{
This function computes the AFS with respect to the given set of individuals
or nodes.
}
\details{
For more information on the format of the result and dimensions, in
particular the interpretation of the first and the last element of the AFS,
please see the tskit manual at
\url{https://tskit.dev/tskit/docs/stable/python-api.html#tskit.TreeSequence.allele_frequency_spectrum}
and the example section dedicated to AFS at
\url{https://tskit.dev/tutorials/analysing_tree_sequences.html#zeroth-and-final-entries-in-the-afs}.
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

samples <- ts_samples(ts) \%>\% .[.$pop \%in\% c("AFR", "EUR"), ]

# compute AFS for the given set of individuals
ts_afs(ts, sample_sets = list(samples$name))
}
