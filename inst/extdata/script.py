import argparse
import os
import sys
import pathlib
import hashlib
import logging
import random

import tskit
import pyslim
import msprime
import pandas
import numpy
import math

VERSION = "__VERSION__"

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
parser.add_argument("--sampling-schedule", metavar="FILE", required=True,
                    help="Path to the slendr sampling schedule table "
                         "(see the manpage of the `sampling()` function for "
                         "more details)")
parser.add_argument("--seed", type=int, help="Random seed value")
parser.add_argument("--verbose", action="store_true", default=False,
                    help="Print detailed logging information?")

args = parser.parse_args()

if args.verbose:
    logging.basicConfig(level=logging.INFO)

if not args.seed:
  args.seed = random.randint(1, sys.maxsize)
  
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
sampling_path = os.path.expanduser(args.sampling_schedule)

# read model configuration files
populations = pandas.read_table(populations_path)
resizes = pandas.DataFrame()
geneflows = pandas.DataFrame()

if os.path.exists(resizes_path):
    resizes = pandas.read_table(resizes_path)
if os.path.exists(geneflows_path):
    geneflows = pandas.read_table(geneflows_path) \
        .rename(columns={"from" : "source"})

length = int(float(open(length_path, "r").readline().rstrip()))

direction = open(direction_path, "r").readline().rstrip()
logging.info(f"Loaded model is specified in the {direction} direction")

description = open(description_path, "r").readline().rstrip()
logging.info(f"Model description: {description}")

if os.path.exists(sampling_path):
    samples_df = pandas.read_table(sampling_path)
else:
    sys.exit(f"Sampling schedule table at '{sampling_path}' does not exist")

logging.info("Loading the sampling schedule")

samples = [
    msprime.SampleSet(
        row.n, population=row.pop, time=length - row.time_gen + 1, ploidy=2
    ) for row in samples_df.itertuples(index=False)
]
if any(sample.num_samples == numpy.inf for sample in samples):
    sys.exit("Please provide the number of individuals to sample")

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
        initial_size = resize_events.tail(1).N[0]
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
    if pop.parent != "ancestor":
        demography.add_population_split(
            time=length - pop.tsplit_gen,
            derived=[pop.pop],
            ancestral=pop.parent
        )

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
        tstart = length - event.tend_gen
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
    logging.info(f"Gene flow from {event.source} to {event.to} between {tstart} and {tend}")
    demography.add_migration_rate_change(
        time=tstart,
        rate=event.rate / (tend - tstart),
        source=event.to,
        dest=event.source,
    )
    demography.add_migration_rate_change(
        time=tend,
        rate=0,
        source=event.to,
        dest=event.source,
    )

# make sure all slendr events are sorted by time of occurence
# (otherwise msprime complains)
demography.sort_events()

if args.verbose:
    print(demography.debug())

logging.info("Running the simulation")

ts = msprime.sim_ancestry(
    samples=samples,
    demography=demography,
    sequence_length=args.sequence_length,
    recombination_rate=args.recombination_rate,
    random_seed=args.seed
)

output_path = os.path.expanduser(args.output)

logging.info(f"Saving tree sequence output to {output_path}")

slendr_metadata = {
    "slendr": {
        "version": "__VERSION__",
        "backend": "msprime",
        "description": description,
        "sampling": {
            "pop" : list(samples_df["pop"].values),
            "n" : list(samples_df["n"].astype(numpy.int32)),
            "time_gen" : list(samples_df["time_gen"].astype(numpy.int32)),
            "x" : list(samples_df["x"].astype(numpy.int32)),
            "y" : list(samples_df["y"].astype(numpy.int32)),
            "time_orig" : list(samples_df["time_orig"].astype(numpy.float32)),
            "x_orig" : list(samples_df["x_orig"].astype(numpy.float32)),
            "y_orig" : list(samples_df["y_orig"].astype(numpy.float32))
        },
        "arguments": {
          "SEQUENCE_LENGTH"   : args.sequence_length,
          "RECOMB_RATE"       : args.recombination_rate,
          "SEED"              : args.seed,
          "SIMULATION_LENGTH" : length
        }
    }
}

tables = ts.dump_tables()
tables.metadata_schema = tskit.MetadataSchema({"codec": "json"})
tables.metadata = slendr_metadata

ts_metadata = tables.tree_sequence()

ts_metadata.dump(output_path)

logging.info("DONE")
