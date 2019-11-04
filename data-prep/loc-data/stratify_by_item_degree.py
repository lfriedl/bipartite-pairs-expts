from __future__ import print_function
from builtins import str, map, range
import numpy as np
import sys

sys.path.append("../../expt-code")
sys.path.append("../../expt-runners")
import loc_data
import prelim_loc_data_expts


def stratify_by_num_edges(adj_mat_infile, edges_infile, outdir, min_edges, max_edges):
    """
    Given a data set, calculate the number of true pairs each item participates in --> its degree.
    Then partition the data set by item degree.
    Write out a separate adj_mat and edges_file for items of each degree, from min_edges to max_edges.
    :param adj_mat_infile: The full bipartite graph for a data set.
    :param edges_infile: Full set of true pairs.
    :param outdir:
    :param min_edges:
    :param max_edges:
    :return:
    """
    edge_matrix, _ = prelim_loc_data_expts.load_edge_matrix(edges_infile) # people are numbered 0 through max in orig file
    adj_mat, row_labels, loc_labels = loc_data.read_loc_adj_mat(adj_mat_infile) # missing some people; row_labels is what matches edge_matrix
    num_friends = np.asarray(edge_matrix.sum(axis=0)).squeeze()

    for edge_count in range(min_edges, max_edges + 1):
        # filter both matrices to people with edge_count
        people_ids_to_keep = set(np.nonzero(num_friends == edge_count)[0])
        print("found " + str(len(people_ids_to_keep)) + " people having " + str(edge_count) + " friends each")

        # save files
        adj_mat_file = outdir + "/bipartite_adj_" + str(edge_count) + "friends.txt"
        edge_mat_file = outdir + "/loc-edges_" + str(edge_count) + "friends.txt"

        with open(adj_mat_file, 'w') as fout:
            # match formatting of orig file: row_id,loc|loc|loc|...
            fout.write("V1,checkins\n")
            for i in range(adj_mat.shape[0]):
                if row_labels[i] in people_ids_to_keep:
                    fout.write(str(row_labels[i]) + "," + "|".join(
                        [loc_labels[j] for j in np.nonzero(adj_mat[i,].toarray()[0])[0]]) + "\n")

        with open(edge_mat_file, 'w') as fout:  # orig format: i<tab>j. (Was symmetric, and we'll store it as such.)
            for (i,j) in zip(*edge_matrix.nonzero()):
                if i in people_ids_to_keep and j in people_ids_to_keep:
                    fout.write(str(i) + "\t" + str(j) + "\n")


if __name__ == "__main__":

    stratify_by_num_edges(adj_mat_infile='/Users/lfriedl/Documents/dissertation/real-data/brightkite/bipartite_adj.txt',
                          edges_infile='/Users/lfriedl/Documents/dissertation/real-data/brightkite/loc-brightkite_edges.txt',
                          outdir='/Users/lfriedl/Documents/dissertation/real-data/brightkite/', min_edges=10, max_edges=10)
