# This module contains a couple of Python functions serving as a link between
# the R interface of slendr and the underlying tskit tree sequence machinery.

import numpy

def mult_roots(ts):
  """Check how many trees in the tree sequence have multiple roots (i.e. how
  many are not fully coalesced.
  """
  return [not tree.has_multiple_roots for tree in ts.trees()]

def get_pedigree_ids(ts):
  """Extract pedigree IDs of all individuals in a SLiM tree sequence.
  """
  # float conversion is an unfortunate hack around Python long int -> R int
  # overflow (fix appears to be a work in progress in reticulate, see here:
  # https://github.com/rstudio/reticulate/issues/323)
  return [float(ind.metadata["pedigree_id"]) for ind in ts.individuals()]
