# This module contains a couple of Python functions serving as a link between
# the R interface of slendr and the underlying tskit tree sequence machinery.
# It is distributed under the same conditions and license as the rest of the
# slendr R package codebase.

import numpy as np
import pandas as pd
import tskit

def __slendr_mult_roots(ts):
  """Check how many trees in the tree sequence have multiple roots (i.e. how
  many are not fully coalesced.
  """
  return [not tree.has_multiple_roots for tree in ts.trees()]

def __slendr_get_pedigree_ids(ts):
  """Extract pedigree IDs of all individuals in a SLiM tree sequence.
  """
  # float conversion is an unfortunate hack around Python long int -> R int
  # overflow (fix appears to be a work in progress in reticulate, see here:
  # https://github.com/rstudio/reticulate/issues/323)
  return [float(ind.metadata["pedigree_id"]) for ind in ts.individuals()]

def __slendr_get_ancestral_states(ts):
  """Extract ancestral states of all mutations in a tree sequence.
  """
  # float conversion is an unfortunate hack around Python long int -> R int
  # overflow (fix appears to be a work in progress in reticulate, see here:
  # https://github.com/rstudio/reticulate/issues/323)
  return [site.ancestral_state for site in ts.sites()]

def __slendr_collect_ibd(ts, within=None, between=None, max_time=None, min_len=0):
    """Extract (squashed) IBD tracts from the given tree sequence"""
    ibd_segments = ts.ibd_segments(
        within=within,
        between=between,
        store_pairs=True,
        store_segments=True,
        min_span=None,
        max_time=max_time
    )

    result = []
    for pair, ibd in ibd_segments.items():
        # sort all segments for the given pair of nodes along the chromosome
        ibd = sorted(ibd, key=lambda segment: segment.left)

        # initialize the information about the first IBD tract
        current_left = ibd[0].left
        current_right = ibd[0].right
        current_mrca = ibd[0].node

        for segment in ibd[1:]:
            next_mrca = segment.node
            # if the MRCA node of the next segment is different than that of
            # the previous segment, the previous IBD tract has ended
            if next_mrca != current_mrca:
                if current_right - current_left > min_len:
                    result.append((pair[0], pair[1], current_mrca, ts.node(current_mrca).time, current_left, current_right))
                # ... then begin recording information about the following IBD
                current_left, current_mrca = segment.left, next_mrca
            # the right end of the current segment is the rightmost possible
            # coordinate of the IBD segment that's being currently collected
            current_right = segment.right

        # finally, record the last (yet unfinished) IBD tract based on the
        # information from the final processed segment
        result.append((pair[0], pair[1], current_mrca, ts.node(current_mrca).time, current_left, segment.right))

    return pd.DataFrame(result, columns=["node1", "node2", "mrca", "tmrca", "left", "right"])
