# Package index

## Compiling and running population genetic models

- [`slim()`](https://bodkan.net/slendr/reference/slim.md) : Run a slendr model
  in SLiM
- [`msprime()`](https://bodkan.net/slendr/reference/msprime.md) : Run a slendr
  model in msprime
- [`compile_model()`](https://bodkan.net/slendr/reference/compile_model.md) :
  Compile a slendr demographic model
- [`read_model()`](https://bodkan.net/slendr/reference/read_model.md) : Read a
  previously serialized model configuration
- [`schedule_sampling()`](https://bodkan.net/slendr/reference/schedule_sampling.md)
  : Define sampling events for a given set of populations
- [`substitute_values()`](https://bodkan.net/slendr/reference/substitute_values.md)
  : Substitute values of parameters in a SLiM extension template

## Installation and configuration of external dependencies

- [`setup_env()`](https://bodkan.net/slendr/reference/setup_env.md) : Setup a
  dedicated Python virtual environment for slendr
- [`init_env()`](https://bodkan.net/slendr/reference/init_env.md) : Activate
  slendr's own dedicated Python environment
- [`check_env()`](https://bodkan.net/slendr/reference/check_env.md) : Check
  that the active Python environment is setup for slendr
- [`clear_env()`](https://bodkan.net/slendr/reference/clear_env.md) : Remove
  the automatically created slendr Python environment
- [`get_python()`](https://bodkan.net/slendr/reference/get_python.md) : Get a
  path to internal Python interpreter of slendr
- [`check_dependencies()`](https://bodkan.net/slendr/reference/check_dependencies.md)
  : Check that the required dependencies are available for slendr to
  work

## Model components

- [`population()`](https://bodkan.net/slendr/reference/population.md) : Define
  a population
- [`world()`](https://bodkan.net/slendr/reference/world.md) : Define a world
  map for all spatial operations

## Spatial population dynamics

- [`move()`](https://bodkan.net/slendr/reference/move.md) : Move the population
  to a new location in a given amount of time
- [`expand_range()`](https://bodkan.net/slendr/reference/expand_range.md) :
  Expand the population range
- [`shrink_range()`](https://bodkan.net/slendr/reference/shrink_range.md) :
  Shrink the population range
- [`set_range()`](https://bodkan.net/slendr/reference/set_range.md) : Update
  the population range
- [`set_dispersal()`](https://bodkan.net/slendr/reference/set_dispersal.md) :
  Change dispersal parameters

## Non-spatial population dynamics

- [`gene_flow()`](https://bodkan.net/slendr/reference/gene_flow.md) : Define a
  gene-flow event between two populations
- [`resize()`](https://bodkan.net/slendr/reference/resize.md) : Change the
  population size

## Manipulation of spatial objects

- [`region()`](https://bodkan.net/slendr/reference/region.md) : Define a
  geographic region

- [`join()`](https://bodkan.net/slendr/reference/join.md) :

  Merge two spatial `slendr` objects into one

- [`overlap()`](https://bodkan.net/slendr/reference/overlap.md) :

  Generate the overlap of two `slendr` objects

- [`subtract()`](https://bodkan.net/slendr/reference/subtract.md) :

  Generate the difference between two `slendr` objects

- [`reproject()`](https://bodkan.net/slendr/reference/reproject.md) : Reproject
  coordinates between coordinate systems

- [`distance()`](https://bodkan.net/slendr/reference/distance.md) : Calculate
  the distance between a pair of spatial boundaries

- [`area()`](https://bodkan.net/slendr/reference/area.md) : Calculate the area
  covered by the given slendr object

## Model visualization and diagnostics

- [`plot_map()`](https://bodkan.net/slendr/reference/plot_map.md) :

  Plot `slendr` geographic features on a map

- [`plot_model()`](https://bodkan.net/slendr/reference/plot_model.md) : Plot
  demographic history encoded in a slendr model

- [`animate_model()`](https://bodkan.net/slendr/reference/animate_model.md) :
  Animate the simulated population dynamics

- [`explore_model()`](https://bodkan.net/slendr/reference/explore_model.md) :
  Open an interactive browser of the spatial model

- [`print(`*`<slendr_pop>`*`)`](https://bodkan.net/slendr/reference/print.slendr_pop.md)
  [`print(`*`<slendr_region>`*`)`](https://bodkan.net/slendr/reference/print.slendr_pop.md)
  [`print(`*`<slendr_map>`*`)`](https://bodkan.net/slendr/reference/print.slendr_pop.md)
  [`print(`*`<slendr_model>`*`)`](https://bodkan.net/slendr/reference/print.slendr_pop.md)
  :

  Print a short summary of a `slendr` object

- [`print(`*`<slendr_ts>`*`)`](https://bodkan.net/slendr/reference/print.slendr_ts.md)
  : Print tskit's summary table of the Python tree-sequence object

- [`summary(`*`<slendr_nodes>`*`)`](https://bodkan.net/slendr/reference/summary.slendr_nodes.md)
  :

  Summarise the contents of a `ts_nodes` result

- [`extract_parameters()`](https://bodkan.net/slendr/reference/extract_parameters.md)
  : Extract information from a compiled model or a simulated tree
  sequence

## Processing tree sequences

- [`ts_read()`](https://bodkan.net/slendr/reference/ts_read.md) : Read a tree
  sequence from a file
- [`ts_write()`](https://bodkan.net/slendr/reference/ts_write.md) : Save a tree
  sequence to a file
- [`ts_recapitate()`](https://bodkan.net/slendr/reference/ts_recapitate.md) :
  Recapitate the tree sequence
- [`ts_simplify()`](https://bodkan.net/slendr/reference/ts_simplify.md) :
  Simplify the tree sequence down to a given set of individuals
- [`ts_mutate()`](https://bodkan.net/slendr/reference/ts_mutate.md) : Add
  mutations to the given tree sequence
- [`ts_samples()`](https://bodkan.net/slendr/reference/ts_samples.md) : Extract
  names and times of individuals of interest in the current tree
  sequence (either all sampled individuals or those that the user
  simplified to)
- [`ts_names()`](https://bodkan.net/slendr/reference/ts_names.md) : Extract
  names of individuals in a tree sequence
- [`ts_coalesced()`](https://bodkan.net/slendr/reference/ts_coalesced.md) :
  Check that all trees in the tree sequence are fully coalesced
- [`ts_save()`](https://bodkan.net/slendr/reference/ts_save.md) : Write a tree
  sequence to a file
- [`ts_load()`](https://bodkan.net/slendr/reference/ts_load.md) : Read a tree
  sequence from a file

## Tree sequence format conversion

- [`ts_genotypes()`](https://bodkan.net/slendr/reference/ts_genotypes.md) :
  Extract genotype table from the tree sequence
- [`ts_eigenstrat()`](https://bodkan.net/slendr/reference/ts_eigenstrat.md) :
  Convert genotypes to the EIGENSTRAT file format
- [`ts_vcf()`](https://bodkan.net/slendr/reference/ts_vcf.md) : Save genotypes
  from the tree sequence as a VCF file

## Accessing tree sequence components

- [`ts_nodes()`](https://bodkan.net/slendr/reference/ts_nodes.md) : Extract
  combined annotated table of individuals and nodes

- [`ts_edges()`](https://bodkan.net/slendr/reference/ts_edges.md) : Extract
  spatio-temporal edge annotation table from a given tree or tree
  sequence

- [`ts_table()`](https://bodkan.net/slendr/reference/ts_table.md) : Get the
  table of individuals/nodes/edges/mutations/sites from the tree
  sequence

- [`ts_phylo()`](https://bodkan.net/slendr/reference/ts_phylo.md) :

  Convert a tree in the tree sequence to an object of the class `phylo`

- [`ts_tree()`](https://bodkan.net/slendr/reference/ts_tree.md) : Get a tree
  from a given tree sequence

- [`ts_draw()`](https://bodkan.net/slendr/reference/ts_draw.md) : Plot a
  graphical representation of a single tree

- [`ts_metadata()`](https://bodkan.net/slendr/reference/ts_metadata.md) :
  Extract list with tree sequence metadata saved by SLiM

- [`ts_ancestors()`](https://bodkan.net/slendr/reference/ts_ancestors.md) :
  Extract (spatio-)temporal ancestral history for given
  nodes/individuals

- [`ts_descendants()`](https://bodkan.net/slendr/reference/ts_descendants.md) :
  Extract all descendants of a given tree-sequence node

## Tree sequence statistics

- [`ts_f2()`](https://bodkan.net/slendr/reference/ts_f4ratio.md)
  [`ts_f3()`](https://bodkan.net/slendr/reference/ts_f4ratio.md)
  [`ts_f4()`](https://bodkan.net/slendr/reference/ts_f4ratio.md)
  [`ts_f4ratio()`](https://bodkan.net/slendr/reference/ts_f4ratio.md) :
  Calculate the f2, f3, f4, and f4-ratio statistics
- [`ts_afs()`](https://bodkan.net/slendr/reference/ts_afs.md) : Compute the
  allele frequency spectrum (AFS)
- [`ts_divergence()`](https://bodkan.net/slendr/reference/ts_divergence.md) :
  Calculate pairwise divergence between sets of individuals
- [`ts_diversity()`](https://bodkan.net/slendr/reference/ts_diversity.md) :
  Calculate diversity in given sets of individuals
- [`ts_fst()`](https://bodkan.net/slendr/reference/ts_fst.md) : Calculate
  pairwise statistics between sets of individuals
- [`ts_tajima()`](https://bodkan.net/slendr/reference/ts_tajima.md) : Calculate
  Tajima's D for given sets of individuals
- [`ts_segregating()`](https://bodkan.net/slendr/reference/ts_segregating.md) :
  Calculate the density of segregating sites for the given sets of
  individuals
- [`ts_ibd()`](https://bodkan.net/slendr/reference/ts_ibd.md) : Collect
  Identity-by-Descent (IBD) segments (EXPERIMENTAL)
- [`ts_tracts()`](https://bodkan.net/slendr/reference/ts_tracts.md) : Extract
  ancestry tracts from a tree sequence (EXPERIMENTAL)
