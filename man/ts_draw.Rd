% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree-sequences.R
\name{ts_draw}
\alias{ts_draw}
\title{Plot a graphical representation of a single tree}
\usage{
ts_draw(
  x,
  width = 1000,
  height = 1000,
  labels = FALSE,
  sampled_only = TRUE,
  title = NULL,
  ...
)
}
\arguments{
\item{x}{A single tree extracted by \code{\link{ts_tree}}}

\item{width, height}{Pixel dimensions of the rendered bitmap}

\item{labels}{Label each node with the individual name?}

\item{sampled_only}{Should only individuals explicitly sampled through
simplification be labeled? This is relevant in situations in which sampled
individuals can themselves be among the ancestral nodes.}

\item{title}{Optional title for the figure}

\item{...}{Keyword arguments to the tskit \code{draw_svg} function.}
}
\value{
No return value, called for side effects
}
\description{
This function first obtains an SVG representation of the tree by calling the
\code{draw_svg} method of tskit and renders it as a bitmap image in R. All of
the many optional keyword arguments of the \code{draw_svg} method can be
provided and will be automatically passed to the method behind the scenes.
}
\examples{
\dontshow{check_dependencies(python = TRUE, quit = TRUE) # dependencies must be present
}
init_env()

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk
ts <- ts_read(slendr_ts, model)

# extract the first tree in the tree sequence and draw it
tree <- ts_tree(ts, i = 1)

# ts_draw accepts various optional arguments of tskit.Tree.draw_svg
ts_draw(tree, time_scale = "rank")
}
