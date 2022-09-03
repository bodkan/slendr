import tskit
import numpy
import tempfile

tables = tskit.TableCollection(sequence_length=1e5)

node_table = tables.nodes
node_table.add_row(flags=tskit.NODE_IS_SAMPLE, individual=0, population=0)  # node 0
node_table.add_row(flags=tskit.NODE_IS_SAMPLE, individual=0, population=0)  # node 1
node_table.add_row(time=3, population=1)                      # node 2
node_table.add_row(flags=tskit.NODE_IS_SAMPLE, individual=1, population=2)  # node 3
node_table.add_row(flags=tskit.NODE_IS_SAMPLE, individual=1, population=2)  # node 4
node_table.add_row(time=7, population=2)                      # node 5
node_table.add_row(time=10, population=1)                     # node 6
node_table

edge_table = tables.edges
edge_table.set_columns(
    left=numpy.array([0, 0, 0, 0, 0, 0]),
    right=numpy.array([1e5, 1e5, 1e5, 1e5, 1e5, 1e5]),
    parent=numpy.array([2, 2, 5, 5, 6, 6], dtype=numpy.int32),
    child=numpy.array([0, 1, 3, 4, 2, 5], dtype=numpy.int32)
)
edge_table

ind_table = tables.individuals
ind_table.add_row()
ind_table.add_row()
ind_table.add_row()
ind_table.add_row()

pop_table = tables.populations
pop_table.add_row()
pop_table.add_row()
pop_table.add_row()

sites_table = tables.sites
sites_table.add_row(position=42, ancestral_state="0")
sites_table.add_row(position=123, ancestral_state="0")

mutations_table = tables.mutations
mutations_table.add_row(site=0, node=2, derived_state="1")
mutations_table.add_row(site=1, node=5, derived_state="1")

tseq = tables.tree_sequence()

filename = tempfile.NamedTemporaryFile().name

tseq.dump(filename)
