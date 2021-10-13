import argparse
import os
import sys
import pathlib
import hashlib

import tskit
import pyslim
import msprime
import pandas

parser = argparse.ArgumentParser("msprime script for executing slendr models")
parser.add_argument("--model", metavar="DIRECTORY", required=True,
                   help="Location of a slendr model directory")
parser.add_argument("--output", metavar="FILE", required=True,
                    help="Tree sequence output path")
parser.add_argument("--seq-length", required=True, type=float,
                    help="Amount of sequence to simulate")
parser.add_argument("--recomb-rate", required=True, type=float, help="Recombination rate")
parser.add_argument("--mutation-rate", default=0.0, type=float, help="Mutation rate")
parser.add_argument("--sampling-schedule", metavar="FILE", required=True,
                    help="Path to table with sampling schedule created by slendr `sampling()`")
parser.add_argument("--seed", type=int, help="Random seed value")

#args = parser.parse_args()
args = parser.parse_args("--model ~/Desktop/test --output ./test.ts --seq-length 1000 --recomb-rate 0 --mutation-rate 0 --sampling-schedule ~/Desktop/test/sampling.tsv".split())

model_dir = os.path.expanduser(args.model)
sampling_schedule = os.path.expanduser(args.sampling_schedule)

if not os.path.exists(model_dir):
    sys.exit(f"Model directory {model_dir} does not exist")

model = {
    "populations" : pathlib.Path(model_dir, "populations.tsv"),
    "resizes"     : pathlib.Path(model_dir, "resizes.tsv"),
    "geneflows"   : pathlib.Path(model_dir, "geneflow.tsv"),
    "length"      : pathlib.Path(model_dir, "length.txt")
}

# read model configuration files
populations = pandas.read_table(model["populations"])
resizes = pandas.read_table(model["resizes"]) if os.path.exists(model["resizes"]) else None
# Python doesn't like the name of the column "from"
if os.path.exists(model["geneflows"]):
    geneflows = pandas.read_table(model["geneflows"]) \
        .rename(columns={"from" : "source"})
else:
    geneflows = None
length = int(float(open(model["length"], "r").readline().rstrip()))

# read the sampling schedule table or create a default one
if os.path.exists(sampling_schedule):
    samples_df = pandas.read_table(sampling_schedule)
else:
    sys.exit(f"Sampling schedule table is missing")

samples = [msprime.SampleSet(n, population=pop, time=length - time_gen + 1, ploidy=2)
                             for (_, pop, n, time_gen, _) in samples_df.itertuples()]

# set up demographic history
demography = msprime.Demography()
for pop in populations.itertuples():
    demography.add_population(name=pop.pop, initial_size=pop.N, initially_active=True)
    # for non-ancestral populations, specify the correct split event
    if (pop.parent != "ancestor"):
        demography.add_population_split(
            time=length - pop.tsplit_gen,
            derived=[pop.pop],
            ancestral=pop.parent
        )

# set gene flow events
for event in geneflows.itertuples():
    demography.set_migration_rate(source=event.to, dest=event.source, rate=event.rate)
    demography.set_migration_rate(source=event.to, dest=event.source, rate=0)

# make sure all slendr events are sorted by time of occurence
# (otherwise msprime complains)
demography.sort_events()

ts = msprime.sim_ancestry(
    samples=samples,
    demography=demography,
    sequence_length=args.seq_length,
    recombination_rate=args.recomb_rate,
    random_seed=args.seed
)

ts.dump(args.output)
