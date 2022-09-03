import tskit
import numpy
import tempfile

tables = tskit.TableCollection(sequence_length=1e5)

node_table = tables.nodes
node_table.add_row(flags=tskit.NODE_IS_SAMPLE)  # node 0
node_table.add_row(flags=tskit.NODE_IS_SAMPLE)  # node 1
node_table.add_row(time=3)                      # node 2
node_table.add_row(flags=tskit.NODE_IS_SAMPLE)  # node 3
node_table.add_row(flags=tskit.NODE_IS_SAMPLE)  # node 4
node_table.add_row(time=7)                      # node 5
node_table.add_row(time=10)                     # node 6
node_table

edge_table = tables.edges
edge_table.set_columns(
    left=numpy.array([0, 0, 0, 0, 0, 0]),
    right=numpy.array([1e5, 1e5, 1e5, 1e5, 1e5, 1e5]),
    parent=numpy.array([2, 2, 5, 5, 6, 6], dtype=numpy.int32),
    child=numpy.array([0, 1, 3, 4, 2, 5], dtype=numpy.int32)
)
edge_table

tseq = tables.tree_sequence()

filename = tempfile.NamedTemporaryFile().name

tseq.dump(filename)
