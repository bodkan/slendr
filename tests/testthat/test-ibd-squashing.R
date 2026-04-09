# this unit test is no longer necessary -- ts_ibd() only produces squashed
# segments, because this is what every user extracting IBD segments wants
# (this script is kept because the manually constructed trees could be
# useful for future IBD testing purposes)

skip_if(TRUE)

skip_if(!check_dependencies(python = TRUE))
init_env(quiet = TRUE)

reticulate::py_run_string("import warnings; import msprime; warnings.simplefilter('ignore', msprime.TimeUnitsMismatchWarning)")

#
# first batch of tests involves a small, manually constructed tree sequence below
#

io <- reticulate::import("io")
tskit <- reticulate::import("tskit")

nodes <- io$StringIO("id is_sample time
0 1 0
1 1 0
2 1 0
3 0 1
4 0 2
5 0 3
6 0 0.8
7 0 0.3
8 0 0.7
9 0 0.2"
)
edges <- io$StringIO("left right parent child
2 8 3 0
8 9 9 0
8 9 3 9
2 9 3 2
9 10 8 0
9 10 8 2
0 8 4 1
8 9 4 6
9 10 4 7
8 9 6 1
9 10 7 1
0 2 4 2
2 8 4 3
8 9 4 3
9 10 4 8
0 2 5 0
0 2 5 4"
)
tmp <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)
tskit$load_text(nodes = nodes, edges = edges, strict = FALSE)$dump(tmp)
