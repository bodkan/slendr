# This Python script serves as an msprime back end of the slendr package.
# As such it is distributed under the same conditions and license as the
# rest of the slendr R package codebase.
#
# The code can be used by the R function `msprime()` in two modes:
#  1. as a standard command-line script, using serialized slendr model data
#  2. as a library called directly from R without serializing models first

import argparse
import os
import sys
import pathlib
import hashlib
import logging

import tskit
import pyslim
import msprime
import pandas
import numpy
import math

VERSION = "__VERSION__"


def simulate(
  sequence_length, recombination_rate, seed,
  populations, resizes, geneflows, length, direction, description,
  samples, debug
):
  """Run a slendr simulation and return a tree sequence."""

  logging.info("Setting up populations")

  # set up demographic history
  demography = msprime.Demography()
  for pop in populations.itertuples():
      # get the initial population size (in backwards direction) -- this is
      # either the last (in forward direction) resize event recorded for the
      # population, or its size after split
      name = pop.pop
      if len(resizes) and name in set(resizes["pop"]):
          resize_events = resizes.query(f"pop == '{name}'")
          initial_size = resize_events.tail(1).N.values[0]
      else:
          initial_size = pop.N

      logging.info(f"Setting up population {pop.pop} with Ne {initial_size}")
      demography.add_population(
          name=pop.pop,
          initial_size=initial_size,
          initially_active=True
      )
      # for non-ancestral populations, specify the correct split event and re-set
      # the effective population size (by default in msprime inherited from the
      # parent population)
      if pop.parent != "__pop_is_ancestor":
          demography.add_population_split(
              time=length - pop.tsplit_gen,
              derived=[pop.pop],
              ancestral=pop.parent
          )

  if len(samples) == 0:
      logging.info("No sampling schedule given, generating one automatically")
      samples = pandas.DataFrame(
          [
            (pop.initial_size, pop.name, length + 1, -1, -1, 0, -1, -1)
            for pop in demography.populations
          ],
          columns=["n", "pop", "time_gen", "x", "y", "time_orig", "x_orig", "y_orig"]
      )

  logging.info("Loading the sampling schedule")
  sample_set = [
      msprime.SampleSet(
         row.n, population=row.pop, time=length - row.time_gen + 1, ploidy=2
      ) for row in samples.itertuples(index=False)
  ]

  logging.info("Setting up population resize events")

  # schedule population size changes
  for event in resizes.itertuples(index=False):
      if event.how == "step":
          time = length - event.tresize_gen
          logging.info(f"Step resize of population {pop.pop} to {event.prev_N} from {event.N} at time {time}")
          demography.add_population_parameters_change(
              time=time,
              initial_size=event.prev_N,
              population=event.pop
          )
      elif event.how == "exponential":
          tstart = length - event.tend_gen + 1
          tend = length - event.tresize_gen
          r = math.log(event.prev_N / event.N) / (tstart - tend)
          logging.info(f"Exponential resize of population {pop.pop} from {event.N} to {event.prev_N}, growth rate {r}, from {tstart} to {tend} generations")
          demography.add_population_parameters_change(
              time=tstart,
              growth_rate=r,
              population=event.pop
          )
          demography.add_population_parameters_change(
              time=tend,
              growth_rate=0,
              population=event.pop
          )
      else:
          sys.exit(f"Unknown event type '{event.how}'")

  logging.info("Setting up gene flow events")

  # schedule gene flow events
  for event in geneflows.itertuples():
      tstart = length - event.tend_gen + 1
      tend = length - event.tstart_gen + 1
      logging.info(f"Gene flow from {event._1} to {event.to} between {tstart} and {tend}")
      demography.add_migration_rate_change(
          time=tstart,
          rate=event.rate / (tend - tstart),
          source=event.to,
          dest=event._1    # sadly, pandas renames 'from' to '_1'
      )
      demography.add_migration_rate_change(
          time=tend,
          rate=0,
          source=event.to,
          dest=event._1,
      )

  # make sure all slendr events are sorted by time of occurence
  # (otherwise msprime complains)
  demography.sort_events()

  if debug:
      print(demography.debug())

  logging.info("Running the simulation and generating a tree sequence")

  ts = msprime.sim_ancestry(
      samples=sample_set,
      demography=demography,
      sequence_length=sequence_length,
      recombination_rate=recombination_rate,
      random_seed=seed
  )

  # symbolic names of individuals that have been recorded in the tree sequence
  # (this vector is used in downstream analyses to keep track of which individuals
  # were kept during the process of ts_simplify() etc.)
  sample_names = []
  for pop in samples["pop"].unique():
      n = sum(samples[samples["pop"] == pop]["n"])
      sample_names += [f"{pop}_{i}" for i in range(1, n + 1)]

  # compile a set of slendr metadata to be stored in the tree sequence
  slendr_metadata = {
      "slendr": {
          "version": "__VERSION__",
          "backend": "msprime",
          "description": description,
          "sampling": {
              "pop" : list(samples["pop"].values),
              "n" : list(samples["n"].astype(numpy.int32)),
              "time_gen" : list(samples["time_gen"].astype(numpy.int32)),
              "x" : list(samples["x"].astype(numpy.int32)),
              "y" : list(samples["y"].astype(numpy.int32)),
              "time_orig" : list(samples["time_orig"].astype(numpy.float32)),
              "x_orig" : list(samples["x_orig"].astype(numpy.float32)),
              "y_orig" : list(samples["y_orig"].astype(numpy.float32))
          },
          "sample_names": sample_names,
          "arguments": {
            "SEQUENCE_LENGTH"   : sequence_length,
            "RECOMB_RATE"       : recombination_rate,
            "SEED"              : seed,
            "SIMULATION_LENGTH" : length
          }
      }
  }

  tables = ts.dump_tables()
  tables.metadata_schema = tskit.MetadataSchema({"codec": "json"})
  tables.metadata = slendr_metadata

  ts_metadata = tables.tree_sequence()

  logging.info("DONE")

  return ts_metadata


if __name__ == "__main__":
  parser = argparse.ArgumentParser(
      "msprime script for executing non-spatial slendr models"
  )
  parser.add_argument("--model", metavar="DIRECTORY", default=".",
                     help="Path to a slendr model directory")
  parser.add_argument("--output", metavar="FILE",
                      help="Path to a tree sequence output file")
  parser.add_argument("--sequence-length", required=True, type=int,
                      help="The length of a sequence to simulate")
  parser.add_argument("--recombination-rate", required=True, type=float,
                      help="Uniform recombination rate")
  parser.add_argument("--sampling-schedule", metavar="FILE",
                      help="Path to the slendr sampling schedule table "
                           "(see the manpage of the `sampling()` function for "
                           "more details)")
  parser.add_argument("--seed", type=int, help="Random seed value")
  parser.add_argument("--verbose", action="store_true", default=False,
                      help="Print detailed logging information?")
  parser.add_argument("--debug", action="store_true", default=False,
                      help="Print detailed debugging information?")

  args = parser.parse_args()

  if args.verbose:
      logging.basicConfig(level=logging.INFO)

  model_dir = os.path.expanduser(args.model)

  logging.info(f"Loading slendr model configuration files from {model_dir}")

  if not args.output:
      args.output = pathlib.Path(model_dir, "output_msprime_ts.trees")

  if not os.path.exists(model_dir):
      sys.exit(f"Model directory {model_dir} does not exist")

  # paths to mandatory slendr configuration files
  populations_path = pathlib.Path(model_dir, "populations.tsv")
  resizes_path = pathlib.Path(model_dir, "resizes.tsv")
  geneflows_path = pathlib.Path(model_dir, "geneflow.tsv")
  length_path = pathlib.Path(model_dir, "length.txt")
  direction_path = pathlib.Path(model_dir, "direction.txt")
  description_path = pathlib.Path(model_dir, "description.txt")

  # read model configuration files
  populations = pandas.read_table(populations_path)
  resizes = pandas.DataFrame()
  geneflows = pandas.DataFrame()
  samples = pandas.DataFrame()

  if os.path.exists(resizes_path):
      resizes = pandas.read_table(resizes_path)
  if os.path.exists(geneflows_path):
      geneflows = pandas.read_table(geneflows_path)

  # read the total simulation length
  length = int(float(open(length_path, "r").readline().rstrip()))

  # read the direction of the model ("forward" or "backward")
  direction = open(direction_path, "r").readline().rstrip()
  logging.info(f"Loaded model is specified in the {direction} direction")

  # read the description of the model
  description = open(description_path, "r").readline().rstrip()
  logging.info(f"Model description: {description}")

  if args.sampling_schedule:
      sampling_path = os.path.expanduser(args.sampling_schedule)
      samples = pandas.read_table(sampling_path)

  ts = simulate(args.sequence_length, args.recombination_rate, args.seed,
                populations, resizes, geneflows, length, direction, description,
                samples, args.debug)

  output_path = os.path.expanduser(args.output)

  logging.info(f"Saving tree sequence output to {output_path}")

  ts.dump(output_path)
