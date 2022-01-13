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

VERSION = "__VERSION__"

parser = argparse.ArgumentParser(
    "msprime script for executing non-spatial slendr models"
)
parser.add_argument("--model", metavar="DIRECTORY", default=".",
                   help="Path to a slendr model directory")
parser.add_argument("--output", metavar="FILE",
                    help="Path to a tree sequence output file")
parser.add_argument("--sequence-length", required=True, type=float,
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
parser.add_argument("--debug", action="store_true", default=False,
                    help="Print a model debugging summary?")

args = parser.parse_args()
# args = parser.parse_args("--model ~/Desktop/msprime-test/ --output ~/Desktop/msprime-test/msprime.trees --sequence-length 100000 --recombination-rate 0 --sampling-schedule ~/Desktop/msprime-test/output_sampling.tsv".split())

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

if os.path.exists(sampling_path):
    samples_df = pandas.read_table(sampling_path)
else:
    sys.exit(f"Sampling schedule table at '{sampling_path}' does not exist")

logging.info(f"Loading the sampling schedule")

samples = [
    msprime.SampleSet(
        row.n, population=row.pop, time=length - row.time_gen + 1, ploidy=2
    ) for row in samples_df.itertuples(index=False)
]
if any(sample.num_samples == numpy.inf for sample in samples):
    sys.exit("Please provide the number of individuals to sample")

logging.info("Setting up an msprime demographic model")

# set up demographic history
demography = msprime.Demography()
for pop in populations.itertuples():
    # get the initial population size (in backwards direction) -- this is
    # either the last (in forward direction) resize event recorded for the
    # population, or its size after split
    name = pop.pop
    if len(resizes):
        resize_events = resizes.query(f"pop == '{name}'")
        initial_size = resize_events.tail(1).N[0]
    else:
        initial_size = pop.N

    if pop.parent == "ancestor":
        demography.add_population(
            name=pop.pop,
            initial_size=initial_size,
            initially_active=True
        )
    # for non-ancestral populations, specify the correct split event and re-set
    # the effective population size (by default in msprime inherited from the
    # parent population)
    else:
        demography.add_population_split(
            time=length - pop.tsplit_gen,
            derived=[pop.pop],
            ancestral=pop.parent
        )
        demography.add_population_parameters_change(
            time=length - pop.tsplit_gen,
            initial_size=initial_size,
            population=pop.pop
        )

# schedule population size changes
for event in resizes.itertuples(index=False):
    if event.how == "step":
        demography.add_population_parameters_change(
            time=length - event.tresize_gen,
            initial_size=event.prev_N,
            population=event.pop
        )
    elif event.how == "exponential":
        r = math.log(event.prev_N / event.N) / (event.tend_gen - event.tresize_gen)
        demography.add_population_parameters_change(
            time=length - event.tresize_gen,
            growth_rate=r,
            population=event.pop
        )
        demography.add_population_parameters_change(
            time=length - event.tresize_gen,
            growth_rate=0,
            population=event.pop
        )
    else:
        sys.exit(f"Unknown event type '{event.how}")

# schedule gene flow events
for event in geneflows.itertuples():
    demography.add_migration_rate_change(
        time=length - event.tend_gen + 1,
        rate=event.rate,
        source=event.to,
        dest=event.source,
    )
    demography.add_migration_rate_change(
        time=length - event.tstart_gen + 1,
        rate=0,
        source=event.to,
        dest=event.source,
    )

# make sure all slendr events are sorted by time of occurence
# (otherwise msprime complains)
demography.sort_events()

if args.debug:
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

ts.dump(output_path)

logging.info("DONE")
