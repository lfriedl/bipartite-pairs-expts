from __future__ import print_function
from builtins import str, map, range
import numpy as np
import random
import tarfile
import gzip
import sys

sys.path.append("../../expt-code")
sys.path.append("../../expt-runners")
import loc_data
import prelim_loc_data_expts

# Notes on formats:
# -We don't expect to see all the nodes in either file. That's why both reader functions tell us how labels
# map to matrix rows.
# -So, node identifiers in both types of files should always be treated as "labels."
# -Only exception: in the .rowIDs files I create, nodes are referred to by their indices in the adj_mat. This has
# the side effect of only sampling nodes that have >0 affils. (Might want to revisit this choice.)

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
    edge_matrix, edge_row_labels_map = prelim_loc_data_expts.load_edge_matrix(edges_infile) # people are numbered 0 through max in orig file
    edge_row_indices_to_labels = {v: k for k, v in edge_row_labels_map.iteritems()}     # simply reversed, var[index] = label

    adj_mat, row_labels, loc_labels = loc_data.read_loc_adj_mat(adj_mat_infile) # missing some people; row_labels is what matches edge_matrix
    num_friends = np.asarray(edge_matrix.sum(axis=0)).squeeze()

    for edge_count in range(min_edges, max_edges + 1):
        # filter both matrices to people with edge_count
        people_to_keep_edgemat_index = set(np.nonzero(num_friends == edge_count)[0])
        print("found " + str(len(people_to_keep_edgemat_index)) + " people having " + str(edge_count) + " friends each")
        people_labels_to_keep = set([label for (ind, label) in edge_row_indices_to_labels.iteritems()
                                     if ind in people_to_keep_edgemat_index])
        # save files
        adj_mat_file = outdir + "/bipartite_adj_" + str(edge_count) + "friends.txt.gz"
        edge_mat_file = outdir + "/loc-edges_" + str(edge_count) + "friends.txt.gz"

        with gzip.open(adj_mat_file, 'w') as fout:
            # match formatting of orig file: row_id,loc|loc|loc|...
            fout.write("V1,checkins\n")
            for i in range(adj_mat.shape[0]):
                if row_labels[i] in people_labels_to_keep:
                    fout.write(str(row_labels[i]) + "," + "|".join(
                        [loc_labels[j] for j in np.nonzero(adj_mat[i,].toarray()[0])[0]]) + "\n")

        with gzip.open(edge_mat_file, 'w') as fout:  # orig format: i<tab>j. (Was symmetric, and we'll store it as such.)
            for (i,j) in zip(*edge_matrix.nonzero()):
                if edge_row_indices_to_labels[i] in people_labels_to_keep and edge_row_indices_to_labels[j] in people_labels_to_keep:
                    fout.write(str(edge_row_indices_to_labels[i]) + "\t" + str(edge_row_indices_to_labels[j]) + "\n")


# Similar to prelim_loc_data_expts.read_sample_save, but reads big files only once, and can print info w/o saving samples
# Note that the IDs in rows_outfile(s) are meant to be interpreted with respect to adj_mat, though its identity isn't stored.
def read_sample_look_save(adj_mat_infile, edges_infile, num_nodes, num_samples, keep_if_min_edges=0, rows_outfile_base_dir=None):
    adj_mat, item_names, _ = prelim_loc_data_expts.read_loc_adj_mat(adj_mat_infile)
    edge_matrix, edge_item_names_map = prelim_loc_data_expts.load_edge_matrix(edges_infile)
    # edge_col_sums = np.asarray(edge_matrix.sum(axis=0)).squeeze()

    j = 0
    while j <= num_samples:
        row_ids_to_keep = set(random.sample(list(range(adj_mat.shape[0])), num_nodes)) # indices w/in adj_mat
        item_names_to_keep = [item_names[i] for i in sorted(row_ids_to_keep)]  # oddly, subset notation above doesn't work

        indices_to_keep_in_edge_mat = [edge_item_names_map[name]
                                       for name in item_names_to_keep if edge_item_names_map.has_key(name)]
        edge_mat_to_keep = edge_matrix[:, indices_to_keep_in_edge_mat][indices_to_keep_in_edge_mat,]
        tot_edges = edge_mat_to_keep.count_nonzero() / 2
        print("Sampled " + str(num_nodes) + " nodes, and found " + str(tot_edges) + " edges among them")

        j += 1

        if rows_outfile_base_dir is not None:
            if tot_edges < keep_if_min_edges:
                print("(retrying)")
                j -= 1
                continue
            filename = rows_outfile_base_dir + '/data' + str(j) + '.rowIDs'
            with open(filename, 'wt') as fp:
                fp.write(" ".join(map(str, sorted(row_ids_to_keep))))   # probably need better syntax

    # Tar them up, to match expected data format
    if rows_outfile_base_dir is not None:
        tar_resfile = rows_outfile_base_dir + "/allInputDataFiles.tgz"
        with tarfile.open(tar_resfile, mode='w:gz') as tf:
            datafiles = ['data' + str(j) + '.rowIDs' for j in range(1, num_samples + 1)]
            [tf.add(rows_outfile_base_dir + "/" + datafile, arcname="./" + datafile) for datafile in datafiles]


# Note: paths correspond to laptop, but files are in different places on the server.
def record_of_runs():
    # In brightkite data:
    # -everyone has at least 1 friendship edge; some have 0 checkins.
    # -friendships: 36% have 1; >1000 people have each of values 2-9 friends; >100 people have each of values 10-32.
    # We'll be further subsetting for indiv trials, so "too big" isn't an issue. Let's do 1-9.
    stratify_by_num_edges(adj_mat_infile='/Users/lfriedl/Documents/dissertation/real-data/brightkite/bipartite_adj.txt',
                          edges_infile='/Users/lfriedl/Documents/dissertation/real-data/brightkite/loc-brightkite_edges.txt',
                          outdir='/Users/lfriedl/Documents/dissertation/real-data/brightkite/stratified', min_edges=1, max_edges=9)
    # Running read_sample_look_save() showed that if we sample 500 nodes from these...
    # data sets of 2friends usually have edges, and it should be pretty secure if we use 6friends or more.

    # Sample 400 baby graphs from each of those stratified layers
    for i in range(6, 10):
        print("looking at items with degree " + str(i))
        read_sample_look_save(
            adj_mat_infile='/Users/lfriedl/Documents/dissertation/real-data/brightkite/stratified/bipartite_adj_' + str(i) + 'friends.txt.gz',
            edges_infile='/Users/lfriedl/Documents/dissertation/real-data/brightkite/stratified/loc-edges_' + str(i) + 'friends.txt.gz',
            num_nodes=500, num_samples=400, keep_if_min_edges=4,
            rows_outfile_base_dir='/Users/lfriedl/Documents/dissertation/binary-ndim/brightkite-expts/stratified_' + str(i) + 'friends/')

    # In gowalla data:
    # 49k people (45%) have 1 friend, but there are NO edges among them (they must all have hubs as friends?)
    # found 30459 people having 2 friends each
    # found 20612 people having 3 friends each
    # found 14925 people having 4 friends each
    # found 11256 people having 5 friends each
    # 6-10 friends: from 8.7k people down to 4k people. (adding degree 10, since we can't use degree 1.)
    stratify_by_num_edges(adj_mat_infile='/Users/lfriedl/Documents/dissertation/real-data/gowalla/bipartite_adj.txt',
                          edges_infile='/Users/lfriedl/Documents/dissertation/real-data/gowalla/loc-gowalla_edges.txt',
                          outdir='/Users/lfriedl/Documents/dissertation/real-data/gowalla/stratified/', min_edges=1, max_edges=10)
    # If we sample 500 nodes, 6friends sometimes yields 0 edges, but 9friends or more may work.
    # (makes sense it would need to be higher, since original # people is higher)
    # --> used 6friends with a min_edges threshold. Up to 8friends sometimes got 0 edges otherwise.

    # Sample 400 baby graphs from each of those stratified layers
    for i in range(6,11):
        print("looking at items with degree " + str(i))
        read_sample_look_save(
            adj_mat_infile = '/Users/lfriedl/Documents/dissertation/real-data/gowalla/stratified/bipartite_adj_' + str(i) + 'friends.txt.gz',
            edges_infile = '/Users/lfriedl/Documents/dissertation/real-data/gowalla/stratified/loc-edges_' + str(i) + 'friends.txt.gz',
            num_nodes=500, num_samples=400, keep_if_min_edges=4,
            rows_outfile_base_dir='/Users/lfriedl/Documents/dissertation/binary-ndim/gowalla-expts/stratified_' + str(i) + 'friends/')




if __name__ == "__main__":

    # stratify_by_num_edges(adj_mat_infile='/Users/lfriedl/Documents/dissertation/real-data/brightkite/bipartite_adj.txt',
    #                       edges_infile='/Users/lfriedl/Documents/dissertation/real-data/brightkite/loc-brightkite_edges.txt',
    #                       outdir='/Users/lfriedl/Documents/dissertation/real-data/brightkite/stratified2', min_edges=9, max_edges=9)
    # for i in range(1, 10):
    #     print("looking at items with degree " + str(i))
    #     read_sample_look_save(
    #         adj_mat_infile='/Users/lfriedl/Documents/dissertation/real-data/brightkite/stratified/bipartite_adj_' + str(i) + 'friends.txt.gz',
    #         edges_infile='/Users/lfriedl/Documents/dissertation/real-data/brightkite/stratified/loc-edges_' + str(i) + 'friends.txt.gz',
    #         num_nodes=500, num_samples=40)
            # num_nodes=500, num_samples=400, keep_if_min_edges=4,
            # rows_outfile_base_dir='/Users/lfriedl/Documents/dissertation/binary-ndim/brightkite-expts/stratified_' + str(i) + 'friends/')

    for i in range(6,10):
        print("looking at items with degree " + str(i))
        read_sample_look_save(
            adj_mat_infile = '/Users/lfriedl/Documents/dissertation/real-data/gowalla/stratified/bipartite_adj_' + str(i) + 'friends.txt.gz',
            edges_infile = '/Users/lfriedl/Documents/dissertation/real-data/gowalla/stratified/loc-edges_' + str(i) + 'friends.txt.gz',
            num_nodes=500, num_samples=400, keep_if_min_edges=4,
            rows_outfile_base_dir='/Users/lfriedl/Documents/dissertation/binary-ndim/gowalla-expts/stratified_' + str(i) + 'friends/')


    # In gowalla data, I don't have the same stats handy, but try doing the same as brightkite
    # stratify_by_num_edges(adj_mat_infile='/Users/lfriedl/Documents/dissertation/real-data/gowalla/bipartite_adj.txt',
    #                       edges_infile='/Users/lfriedl/Documents/dissertation/real-data/gowalla/loc-gowalla_edges.txt',
    #                       outdir='/Users/lfriedl/Documents/dissertation/real-data/gowalla/stratified', min_edges=1, max_edges=10)
    # for i in range(2,11):
    #     print("looking at items with degree " + str(i))
    #     read_sample_look_save(
    #         adj_mat_infile = '/Users/lfriedl/Documents/dissertation/real-data/gowalla/stratified/bipartite_adj_' + str(i) + 'friends.txt',
    #         edges_infile = '/Users/lfriedl/Documents/dissertation/real-data/gowalla/stratified/loc-edges_' + str(i) + 'friends.txt',
    #         num_nodes=500, num_samples=50)
    #         num_nodes=500, num_samples=400,
    #         rows_outfile_base='/Users/lfriedl/Documents/dissertation/binary-ndim/gowalla-expts/stratified_' + str(i) + 'friends/data')

    # for i in range(1,10):
    #     print("looking at items with degree " + str(i))
    #     read_sample_look_save(
    #         adj_mat_infile = '/Users/lfriedl/Documents/dissertation/real-data/brightkite/stratified/bipartite_adj_' + str(i) + 'friends.txt',
    #         edges_infile = '/Users/lfriedl/Documents/dissertation/real-data/brightkite/stratified/loc-edges_' + str(i) + 'friends.txt',
    #         num_nodes=500, num_samples=400,
    #         rows_outfile_base='/Users/lfriedl/Documents/dissertation/binary-ndim/brightkite-expts/stratified_' + str(i) + 'friends/data')
