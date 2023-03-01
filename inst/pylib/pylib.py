# This module contains a couple of Python functions serving as a link between
# the R interface of slendr and the underlying tskit tree sequence machinery.
# It is distributed under the same conditions and license as the rest of the
# slendr R package codebase.

import numpy
import tskit

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

def collect_ibd(ts, coordinates = False, within=None, between=None,
               min_span=None, max_time=None):
    """Extract IBD fragments (or the summary of pairwise IBD sharing) from
    a tree sequence.
    """
    ibd_segments = ts.ibd_segments(
        within=within,
        between=between,
        store_pairs=True,
        store_segments=coordinates,
        min_span=min_span,
        max_time=max_time
    )

    if ibd_segments.num_pairs == 0: return None

    result = []
    for pair, ibd in ibd_segments.items():
        if coordinates:
            node1 = numpy.repeat(pair[0], len(ibd))
            node2 = numpy.repeat(pair[1], len(ibd))
            left = numpy.fromiter((i.left for i in ibd), dtype=int)
            right = numpy.fromiter((i.right for i in ibd), dtype=int)
            pair_result = numpy.column_stack((left, right, right - left, node1, node2)).astype(int)
        else:
            pair_result = numpy.asarray([len(ibd), ibd.total_span, pair[0], pair[1]], dtype=int)
        result.append(pair_result)
    result = numpy.vstack(result)

    return result
