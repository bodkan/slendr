# This module contains a couple of Python functions serving as a link between
# the R interface of slendr and the underlying tskit tree sequence machinery.
# It is distributed under the same conditions and license as the rest of the
# slendr R package codebase.

import numpy as np
import pandas as pd
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
               min_span=None, max_time=None, squash=False):
    """Extract IBD fragments (or the summary of pairwise IBD sharing) from
    a tree sequence.
    """
    # ts = r.ts
    # coordinates = False; within=None; between=None;
    # min_span=3e6; max_time=None; squash=True
    ibd_segments = ts.ibd_segments(
        within=within,
        between=between,
        store_pairs=True,
        store_segments=coordinates,
        min_span=min_span,
        max_time=max_time
    )
    result = []
    for pair, ibd in ibd_segments.items():
        if coordinates:
            # sort all IBD segments between a given pair of nodes based on their position
            # along the chromosome
            ibd = sorted(ibd, key=lambda segment: segment.left)
            if squash:
                curr_left = ibd[0].left
                curr_right = ibd[0].right
                curr_mrca = ibd[0].node
                for segment in ibd:
                    next_mrca = segment.node
                    next_tmrca = ts.node(next_mrca).time
                    if next_mrca != curr_mrca:
                        result.append((pair[0], pair[1],
                                       curr_mrca, ts.node(curr_mrca).time, curr_left, curr_right))
                        curr_left, curr_right, curr_mrca = segment.left, segment.right, next_mrca
                result.append((pair[0], pair[1],
                               curr_mrca, ts.node(curr_mrca).time, curr_left, segment.right))
            else:
                for segment in ibd:
                    mrca = segment.node
                    tmrca = ts.node(mrca).time
                    result.append((pair[0], pair[1],
                                mrca, tmrca, segment.left, segment.right))
        else:
            result.append((pair[0], pair[1], len(ibd), ibd.total_span))

    if coordinates:
        columns = ["node1", "node2", "mrca", "tmrca", "left", "right"]
    else:
        columns = ["node1", "node2", "count", "total"]

    return pd.DataFrame(result, columns=columns)
