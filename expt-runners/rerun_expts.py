
import numpy as np
from timeit import default_timer as timer
import sys
import tarfile
import gzip
from scipy.io import mmread
import re
import glob

import affil_subsets
import prelim_loc_data_expts
sys.path.append("../python-scoring")
import experiment_runner
import scoring_methods
import score_data


sys.path.append("../expt-code")
from loc_data import read_loc_adj_mat  # note: would move this fn here, except that it's needed by the other repo's tests


def synthetic_expts(base_dir, stored_pi_vector_file_gz):
    # read pi_vectors
    pi_vectors = np.loadtxt(stored_pi_vector_file_gz, delimiter=",")

    methods = set(scoring_methods.all_defined_methods) - {'weighted_corr_exp'} # no exponential model

    # for each of the 79 settings, construct its name, then call score_whole_directory on it
    for setting_num in range(1, 80):
        start = timer()
        current_pi_vector = pi_vectors[setting_num - 1]
        data_dir = base_dir + "/pi" + str(setting_num) + "t0.2"
        # (don't remove boundary affils, in case that affects Jaccard or anyone. I think I proved otherwise to myself, but straightforward version is better.)
        experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_jan28", pi_vector_to_use=current_pi_vector,
                                                run_in_parallel=True, remove_boundary_items=False, remove_boundary_affils=False,
                                                sims_for_mixed_pairs=[.2], method_spec=methods, save_pair_scores=False, prefer_faiss=True)
        end = timer()
        print("Ran " + data_dir + " in " + str(end - start) + " secs\n\n\n")

        # also flipped!
        start = timer()
        experiment_runner.score_whole_directory(data_dir, inference_dir_name="infFlip_jan28", flip_high_ps=True, pi_vector_to_use=current_pi_vector,
                                                run_in_parallel=True, remove_boundary_items=False, remove_boundary_affils=False,
                                                sims_for_mixed_pairs=[.2], method_spec=methods, save_pair_scores=False, prefer_faiss=True)
        end = timer()
        print("Ran " + data_dir + " flipped in " + str(end - start) + " secs\n\n\n")



def real_data_newsgroups(base_dir):
    settings = ['alt.atheism', 'comp.graphics', 'comp.os.ms-windows.misc', 'comp.sys.ibm.pc.hardware',
                'comp.sys.mac.hardware', 'comp.windows.x', 'misc.forsale', 'rec.autos', 'rec.motorcycles',
                'rec.sport.baseball', 'rec.sport.hockey', 'sci.crypt', 'sci.electronics', 'sci.med', 'sci.space',
                'soc.religion.christian', 'talk.politics.guns', 'talk.politics.mideast', 'talk.politics.misc',
                'talk.religion.misc']
    mixed_pairs_sims = [.001, .005, .01, .05, .1, .2, .3, .4, .5]
    for dir in settings:
        data_dir = base_dir + "/" + dir
        start = timer()
        experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_feb13",
                                               run_in_parallel=True, remove_boundary_items=False,
                                               save_pair_scores=False, sims_for_mixed_pairs=mixed_pairs_sims, prefer_faiss=True,
                                               affil_subset_every_1_4=True)
        end = timer()
        print("Ran " + data_dir + " in " + str(end - start) + " secs\n\n\n")

        # also flipped!
        start = timer()
        experiment_runner.score_whole_directory(data_dir, inference_dir_name="infFlip_feb13", flip_high_ps=True,
                                                run_in_parallel=True, remove_boundary_items=False,
                                                save_pair_scores=False, sims_for_mixed_pairs=mixed_pairs_sims, prefer_faiss=True,
                                                affil_subset_every_1_4=True)
        end = timer()
        print("Ran " + data_dir + " flipped in " + str(end - start) + " secs\n\n\n")


def real_data_newsgroups_affil_subsets(base_dir):
    settings = ['alt.atheism', 'comp.graphics', 'comp.os.ms-windows.misc', 'comp.sys.ibm.pc.hardware',
                'comp.sys.mac.hardware', 'comp.windows.x', 'misc.forsale', 'rec.autos', 'rec.motorcycles',
                'rec.sport.baseball', 'rec.sport.hockey', 'sci.crypt', 'sci.electronics', 'sci.med', 'sci.space',
                'soc.religion.christian', 'talk.politics.guns', 'talk.politics.mideast', 'talk.politics.misc',
                'talk.religion.misc']
    mixed_pairs_sims = [.001]
    methods = set(scoring_methods.all_defined_methods) - {'weighted_corr_exp'}
    for dir in settings:
        data_dir = base_dir + "/" + dir
        # First, load a file with phi
        phi_to_use = get_a_phi(data_dir)

        # 3 subsets
        start = timer()
        experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_subsetRand.25",
                                                run_in_parallel=True, remove_boundary_items=False,
                                                save_pair_scores=False, sims_for_mixed_pairs=mixed_pairs_sims, prefer_faiss=True,
                                                pi_vector_to_use=phi_to_use, affil_subset_every_1_4=True,
                                                affil_subset_fraction=0.25, affil_subset_type=1)
        end = timer()
        print("Ran rand subset of " + data_dir + " in " + str(end - start) + " secs\n\n\n")
        start = timer()
        experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_subsetMax.25",
                                                run_in_parallel=True, remove_boundary_items=False,
                                                save_pair_scores=False, sims_for_mixed_pairs=mixed_pairs_sims, prefer_faiss=True,
                                                pi_vector_to_use=phi_to_use, affil_subset_every_1_4=True,
                                                affil_subset_fraction=0.25, affil_subset_type=2)
        end = timer()
        print("Ran max subset of " + data_dir + " in " + str(end - start) + " secs\n\n\n")
        start = timer()
        experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_subsetMin.25",
                                                run_in_parallel=True, remove_boundary_items=False,
                                                save_pair_scores=False, sims_for_mixed_pairs=mixed_pairs_sims, prefer_faiss=True,
                                                pi_vector_to_use=phi_to_use, affil_subset_every_1_4=True,
                                                affil_subset_fraction=0.25, affil_subset_type=3)
        end = timer()
        print("Ran min subset of " + data_dir + " in " + str(end - start) + " secs\n\n\n")


def get_a_phi(data_dir):
    phi_file = "data1_phi.txt.gz"
    tar_infile = data_dir + "/allInputDataFiles.tgz"
    tf = tarfile.open(tar_infile)
    members = [tf.getmember("./" + phi_file)]
    tf.extractall(path=data_dir, members=members)
    phi_to_use = np.loadtxt(data_dir + '/' + phi_file)
    return(phi_to_use)


def newsgroups_compute_corrs():
    adj_mat_dir = '/home/lfriedl/ASOUND-bipartite/data-prep/newsgroups/newsgroupAdjacencyMatrices'
    true_pairs_dir = '/home/lfriedl/ASOUND-bipartite/data-prep/newsgroups/20_newsgroup.pairs'
    groups = ['alt.atheism', 'comp.graphics', 'comp.os.ms-windows.misc', 'comp.sys.ibm.pc.hardware',
                'comp.sys.mac.hardware', 'comp.windows.x', 'misc.forsale', 'rec.autos', 'rec.motorcycles',
                'rec.sport.baseball', 'rec.sport.hockey', 'sci.crypt', 'sci.electronics', 'sci.med', 'sci.space',
                'soc.religion.christian', 'talk.politics.guns', 'talk.politics.mideast', 'talk.politics.misc',
                'talk.religion.misc']
    for gp in groups:
        adj_mat_file = adj_mat_dir + '/' + gp + '.mtx.gz'
        with gzip.open(adj_mat_file, 'r') as fp_mm:
            adj_mat = mmread(fp_mm).astype(int, copy=False)  # creates a scipy.sparse.coo_matrix

        rownames_file = adj_mat_dir + '/' + gp + '.itemnames'
        with open(rownames_file) as fin:
            row_labels = [int(line.rstrip()) for line in fin]

        true_pairs_file = true_pairs_dir + '/' + gp + '.pairsGE10'
        edge_matrix, edge_row_labels_map = prelim_loc_data_expts.load_edge_matrix(true_pairs_file, row_labels)

        # now we have adj_mat and edge_matrix, hopefully with the same rows
        friends_per_item = adj_mat.sum(axis=1)
        affils_per_item = edge_matrix.sum(axis=1)

        correlation = np.corrcoef(friends_per_item, affils_per_item, rowvar=False)[0,1]
        print("correlation: " + str(correlation) + " for group " + gp)


def reality_compute_corrs():
    adj_mat_dir = '/home/lfriedl/ASOUND-bipartite/data-prep/reality-mining/realityAdjacencyMatrices'
    settings = ['allPairs-appsByDay', 'allPairs-appsByWeek', 'allPairs-bluetoothByDay', 'allPairs-bluetoothByWeek',
                'allPairs-cellTowersByDay', 'allPairs-cellTowersByWeek']
    for gp in settings:
        adj_mat_file = adj_mat_dir + '/' + gp + '.mtx.gz'
        with gzip.open(adj_mat_file, 'r') as fp_mm:
            adj_mat = mmread(fp_mm).astype(int, copy=False)  # creates a scipy.sparse.coo_matrix
            adj_mat = adj_mat.tocsr()

        rownames_file = adj_mat_dir + '/' + gp + '.itemnames'
        with open(rownames_file) as fin:
            row_labels = [line.rstrip() for line in fin]

        # construct vector of people-IDs. (parse it out of the rownames: it's the number before the underscore)
        person_ids = []
        for row_name in row_labels:
            underscore_at = row_name.find('_')
            person_ids.append(row_name[:underscore_at])

        # going by unique person-ID,
        # -count their occurrences, to compute number of pairs they're part of
        # -locate the rows that apply to them, to compute average affils in those rows
        uniq_people = list(set(person_ids))     # pulling this out for debugging
        num_pairs = []
        avg_affils = []
        for person in uniq_people:
            count_me = person_ids.count(person)
            num_pairs.append(count_me * (count_me + 1) / 2)
            adj_mat_indices = [ind for ind, val in enumerate(person_ids) if val==person]
            avg_affils.append(adj_mat[adj_mat_indices,].sum() / float(count_me))

        # If those are right, then we want their correlation
        correlation = np.corrcoef(num_pairs, avg_affils, rowvar=False)[0, 1]
        print("correlation: " + str(correlation) + " for reality setting " + gp)


def real_data_reality(base_dir):
    settings = ['allPairs-appsByDay', 'allPairs-appsByWeek', 'allPairs-bluetoothByDay', 'allPairs-bluetoothByWeek',
                'allPairs-cellTowersByDay', 'allPairs-cellTowersByWeek']
    mixed_pairs_sims = [.001, .005, .01, .05, .1, .2, .3, .4, .5]
    for dir in settings:
        data_dir = base_dir + "/" + dir
        #start = timer()
        #experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_feb13",
        #                                        run_in_parallel=True, remove_boundary_items=False,
        #                                        save_pair_scores=False, sims_for_mixed_pairs=mixed_pairs_sims, prefer_faiss=True)
        #end = timer()
        #print("Ran " + data_dir + " in " + str(end - start) + " secs\n\n\n")

        # also flipped!
        start = timer()
        experiment_runner.score_whole_directory(data_dir, inference_dir_name="infFlip_feb13", flip_high_ps=True,
                                                run_in_parallel=True, remove_boundary_items=False,
                                                save_pair_scores=False, sims_for_mixed_pairs=mixed_pairs_sims, prefer_faiss=True)
        end = timer()
        print("Ran " + data_dir + " flipped in " + str(end - start) + " secs\n\n\n")


def real_data_reality_affil_subsets(base_dir):
    settings = ['allPairs-appsByDay', 'allPairs-appsByWeek', 'allPairs-bluetoothByDay', 'allPairs-bluetoothByWeek',
                'allPairs-cellTowersByDay', 'allPairs-cellTowersByWeek']
    #settings = ['allPairs-appsByDay']       # temporary!
    mixed_pairs_sims = [.001]
    methods = set(scoring_methods.all_defined_methods) - {'weighted_corr_exp'}  # no exponential model
    for dir in settings:
        data_dir = base_dir + "/" + dir
        # First, load a file with phi
        phi_to_use = get_a_phi(data_dir)

        # 3 subsets
        start = timer()
        experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_subsetRand.25",
                                               run_in_parallel=True, remove_boundary_items=False, method_spec=methods,
                                               save_pair_scores=False, sims_for_mixed_pairs=mixed_pairs_sims, prefer_faiss=True,
                                               pi_vector_to_use=phi_to_use, affil_subset_fraction=0.25, affil_subset_type=1)
        end = timer()
        print("Ran rand subset of " + data_dir + " in " + str(end - start) + " secs\n\n\n")
        start = timer()
        experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_subsetMax.25",
                                               run_in_parallel=True, remove_boundary_items=False, method_spec=methods,
                                               save_pair_scores=False, sims_for_mixed_pairs=mixed_pairs_sims, prefer_faiss=True,
                                               pi_vector_to_use=phi_to_use, affil_subset_fraction=0.25, affil_subset_type=2)
        end = timer()
        print("Ran max subset of " + data_dir + " in " + str(end - start) + " secs\n\n\n")
        start = timer()
        experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_subsetMin.25",
                                                run_in_parallel=True, remove_boundary_items=False, method_spec=methods,
                                                save_pair_scores=False, sims_for_mixed_pairs=mixed_pairs_sims, prefer_faiss=True,
                                                pi_vector_to_use=phi_to_use, affil_subset_fraction=0.25, affil_subset_type=3)
        end = timer()
        print("Ran min subset of " + data_dir + " in " + str(end - start) + " secs\n\n\n")


def congress_all_pairs(base_dir_expts, adj_mat_dir, flip=False, special_extra_s=False):
    parties = ['dem', 'rep']
    sessions = range(110,114)
    mixed_pairs_sims = [.001, .005, .01, .05, .1, .2, .3, .4, .5]
    methods = set(scoring_methods.all_defined_methods) - {'weighted_corr_exp'}
    if special_extra_s:     # just a few settings where we need to try additional s_hats (was still increasing at .5)
        parties = ['rep']
        sessions = [110, 111]
        mixed_pairs_sims = [.001, .005, .01, .05, .1, .2, .3, .4, .5, .6, .7, .8, .9]

    for party in parties:
        for sess in sessions:
            adj_mat_infile = adj_mat_dir + "/" + party + "Votes" + str(sess) + ".mtx.gz"
            adj_mat = score_data.load_adj_mat(adj_mat_infile)

            true_pairs_infile = glob.glob(adj_mat_dir + "/" + party + "Cospons" + str(sess) + "GE*.mtx")[0]
            with open(true_pairs_infile) as fin:
                true_pairs_mat = mmread(fin).astype(int, copy=False).tocsc()
            if true_pairs_mat.shape[0] != true_pairs_mat.shape[1] and true_pairs_mat.shape[0] != adj_mat.shape[0]:
                print("error in matrix sizes for " + adj_mat_infile + " and " + true_pairs_infile)
                return

            # create label generator that uses this matrix.
            def get_true_labels_given_my_edges(pairs_generator):
                return experiment_runner.get_true_labels_from_matrix(pairs_generator, true_pairs_mat)

            if flip:
                evals_outfile = base_dir_expts + "/resultsFlip_" + party + str(sess) + ".txt"
                pair_scores_outfile = base_dir_expts + "/scoredPairsFlip_" + party + str(sess) + ".csv.gz"
            else:
                evals_outfile = base_dir_expts + "/results_" + party + str(sess) + ".txt"
                pair_scores_outfile = base_dir_expts + "/scoredPairs_" + party + str(sess) + ".csv.gz"

            score_data.run_and_eval(adj_mat, true_labels_func=get_true_labels_given_my_edges,
                                    method_spec=methods,
                                    evals_outfile=evals_outfile,
                                    pair_scores_outfile=pair_scores_outfile,
                                    print_timing=True, prefer_faiss=True,
                                    mixed_pairs_sims=mixed_pairs_sims,
                                    flip_high_ps=flip,
                                    remove_boundary_items=False,
                                    remove_boundary_affils=True)    # some votes are unanimous w/in party


def congress_affil_subsets(base_dir_expts, adj_mat_dir):
    parties = ['dem', 'rep']
    sessions = range(110,114)
    mixed_pairs_best_sims = [0.05, 0.001, 0.001, 0.001, 0.5, 0.7, 0.4, 0.2] # determined by manual inspection
    mixed_pairs_best_sims.reverse()      # so that we can .pop() to get them in orig order
    methods = set(scoring_methods.all_defined_methods) - {'weighted_corr_exp'}

    for party in parties:
        for sess in sessions:
            adj_mat_infile = adj_mat_dir + "/" + party + "Votes" + str(sess) + ".mtx.gz"
            adj_mat = score_data.load_adj_mat(adj_mat_infile)

            true_pairs_infile = glob.glob(adj_mat_dir + "/" + party + "Cospons" + str(sess) + "GE*.mtx")[0]
            with open(true_pairs_infile) as fin:
                true_pairs_mat = mmread(fin).astype(int, copy=False).tocsc()

            # create label generator that uses this matrix.
            def get_true_labels_given_my_edges(pairs_generator):
                return experiment_runner.get_true_labels_from_matrix(pairs_generator, true_pairs_mat)

            mixed_pairs_sim = [mixed_pairs_best_sims.pop()]

            tmp_pi_vector = np.asarray(adj_mat.sum(axis=0)).squeeze() / float(adj_mat.shape[0])
            # N.B. Decided to use original pi_vector here, including boundary affils, when choosing affil_subsets.
            # (Some items will end up all-0.)
            # This is different than when using sampled graphs. There, there's a universal pi_vector (always > 0), and your
            # data set might not see some affils. Here, even the universal pi_vector can be 0.

            # affil subset #1: subsetRand.25
            evals_outfile = base_dir_expts + "/results_" + party + str(sess) + "_subsetRand.25.txt"
            pair_scores_outfile = base_dir_expts + "/scoredPairs_" + party + str(sess) + "_subsetRand.25.csv.gz"
            affils_to_keep = affil_subsets.compute_affil_subsets(tmp_pi_vector, affil_subset_fraction=.25,
                                                                 affil_subset_type=1)
            adj_mat_to_use = adj_mat[:, affils_to_keep]
            score_data.run_and_eval(adj_mat_to_use, true_labels_func=get_true_labels_given_my_edges, method_spec=methods,
                                    evals_outfile=evals_outfile, pair_scores_outfile=pair_scores_outfile,
                                    print_timing=True, prefer_faiss=True, mixed_pairs_sims=mixed_pairs_sim,
                                    remove_boundary_items=False, remove_boundary_affils=True)

            # affil subset #2: subsetMax.25
            evals_outfile = base_dir_expts + "/results_" + party + str(sess) + "_subsetMax.25.txt"
            pair_scores_outfile = base_dir_expts + "/scoredPairs_" + party + str(sess) + "_subsetMax.25.csv.gz"
            affils_to_keep = affil_subsets.compute_affil_subsets(tmp_pi_vector, affil_subset_fraction=.25,
                                                                 affil_subset_type=2)
            adj_mat_to_use = adj_mat[:, affils_to_keep]
            score_data.run_and_eval(adj_mat_to_use, true_labels_func=get_true_labels_given_my_edges, method_spec=methods,
                                    evals_outfile=evals_outfile, pair_scores_outfile=pair_scores_outfile,
                                    print_timing=True, prefer_faiss=True, mixed_pairs_sims=mixed_pairs_sim,
                                    remove_boundary_items=False, remove_boundary_affils=True)

            # affil subset #3: subsetMin.25
            evals_outfile = base_dir_expts + "/results_" + party + str(sess) + "_subsetMin.25.txt"
            pair_scores_outfile = base_dir_expts + "/scoredPairs_" + party + str(sess) + "_subsetMin.25.csv.gz"
            affils_to_keep = affil_subsets.compute_affil_subsets(tmp_pi_vector, affil_subset_fraction=.25,
                                                                 affil_subset_type=3)
            adj_mat_to_use = adj_mat[:, affils_to_keep]
            score_data.run_and_eval(adj_mat_to_use, true_labels_func=get_true_labels_given_my_edges, method_spec=methods,
                                    evals_outfile=evals_outfile, pair_scores_outfile=pair_scores_outfile,
                                    print_timing=True, prefer_faiss=True, mixed_pairs_sims=mixed_pairs_sim,
                                    remove_boundary_items=False, remove_boundary_affils=True)


def congress_correlations(adj_mat_dir, flip=False):
    parties = ['dem', 'rep']
    sessions = range(110,114)
    for party in parties:
        for sess in sessions:
            adj_mat_infile = adj_mat_dir + "/" + party + "Votes" + str(sess) + ".mtx.gz"
            adj_mat = score_data.load_adj_mat(adj_mat_infile)

            true_pairs_infile = glob.glob(adj_mat_dir + "/" + party + "Cospons" + str(sess) + "GE*.mtx")[0]
            with open(true_pairs_infile) as fin:
                true_pairs_mat = mmread(fin).astype(int, copy=False).tocsc()

            # now we have adj_mat and true_pairs_mat, hopefully with the same rows
            friends_per_item = true_pairs_mat.sum(axis=1)
            if flip:
                affil_degrees = np.asarray(adj_mat.sum(axis=0)).squeeze()
                cmpts_to_flip = affil_degrees > (.5 * adj_mat.shape[0])
                which_nonzero = np.nonzero(cmpts_to_flip)
                adj_mat[:, which_nonzero[0]] = np.ones(adj_mat[:, which_nonzero[0]].shape, dtype=adj_mat.dtype) \
                                                   - adj_mat[:, which_nonzero[0]]
            affils_per_item = adj_mat.sum(axis=1)
            correlation = np.corrcoef(friends_per_item, affils_per_item, rowvar=False)[0, 1]
            print("correlation: " + str(correlation) + " for " + party + str(sess) + (" flipped" if flip else ""))



def record_of_runs():

    stored_pi_vector_file_gz = '/home/lfriedl/bipartite-pairs-expts/data-prep/synthetic_data/params/pi_vectors.csv.gz'
    base_dir = '/home/lfriedl/ASOUND-bipartite/expts/synth/new1000'
    synthetic_expts(base_dir, stored_pi_vector_file_gz)

    base_dir = '/home/lfriedl/ASOUND-bipartite/expts/newsgroups/new'
    real_data_newsgroups(base_dir)
    real_data_newsgroups_affil_subsets(base_dir)

    base_dir = '/home/lfriedl/ASOUND-bipartite/expts/reality-mining/new1000'
    real_data_reality(base_dir)
    real_data_reality_affil_subsets(base_dir)

    # (forgot to set method_spec and sims_for_mixed_pairs in a lot of runs, oh well.)

    congress_all_pairs(base_dir_expts='/Users/lfriedl/Documents/dissertation/binary-ndim/data2020/congress',
                       adj_mat_dir='/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs-expts/data-prep/congress/adjacency-matrices',
                       flip=False)
    congress_all_pairs(base_dir_expts='/Users/lfriedl/Documents/dissertation/binary-ndim/data2020/congress',
                       adj_mat_dir='/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs-expts/data-prep/congress/adjacency-matrices',
                       flip=False, special_extra_s=True)
    congress_all_pairs(base_dir_expts='/Users/lfriedl/Documents/dissertation/binary-ndim/data2020/congress',
                       adj_mat_dir='/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs-expts/data-prep/congress/adjacency-matrices',
                       flip=True)
    congress_correlations(adj_mat_dir='/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs-expts/data-prep/congress/adjacency-matrices',
                       flip=False)
    congress_correlations(adj_mat_dir='/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs-expts/data-prep/congress/adjacency-matrices',
                       flip=True)
    congress_affil_subsets(base_dir_expts='/Users/lfriedl/Documents/dissertation/binary-ndim/data2020/congress',
                           adj_mat_dir='/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs-expts/data-prep/congress/adjacency-matrices')


if __name__ == "__main__":
    #base_dir = '/home/lfriedl/ASOUND-bipartite/expts/newsgroups/new'
    #real_data_newsgroups(base_dir)  # redo the flipped part of NG
    #exit(0)
    base_dir = '/home/lfriedl/ASOUND-bipartite/expts/reality-mining/new1000'
    real_data_reality_affil_subsets(base_dir)
    exit(0)
    #real_data_reality(base_dir)
    #exit(0)

    # testing/debugging
    methods = set(scoring_methods.all_defined_methods) - {'weighted_corr_exp'}  # no exponential model
    data_dir = '/home/lfriedl/ASOUND-bipartite/expts/reality-mining/new1000/allPairs-appsByDay'
    phi_to_use = get_a_phi(data_dir)
    experiment_runner.score_data_set(data_dir, 1, inference_dir_name="test_feb13",
                                            remove_boundary_items=False, method_spec=methods,
                                            save_pair_scores=False, sims_for_mixed_pairs=[.001], prefer_faiss=True,
                                            pi_vector_to_use=phi_to_use, affil_subset_fraction=0.25, affil_subset_type=3)
    exit(0)

    data_dir = '/home/lfriedl/ASOUND-bipartite/expts/newsgroups/new/alt.atheism'
    phi_to_use = get_a_phi(data_dir)
    experiment_runner.score_data_set(data_dir, 1, inference_dir_name="test_feb13", pi_vector_to_use=phi_to_use,
                                     verbose=True, save_pair_scores=False, sims_for_mixed_pairs=[.001],
                                     remove_boundary_items=False, remove_boundary_affils=True,
                                     method_spec=methods, prefer_faiss=True,
                                     affil_subset_every_1_4=True,
                                     affil_subset_fraction=0.25, affil_subset_type=1)



    stored_pi_vector_file_gz = '/home/lfriedl/bipartite-pairs-expts/data-prep/synthetic_data/params/pi_vectors.csv.gz'
    pi_vectors = np.loadtxt(stored_pi_vector_file_gz, delimiter=",")

    setting_num = 21
    current_pi_vector = pi_vectors[setting_num - 1]
    data_dir = base_dir + "/pi" + str(setting_num) + "t0.2"
    experiment_runner.score_data_set(data_dir, 1, inference_dir_name="test_feb12", pi_vector_to_use=current_pi_vector,
                                     verbose=True, save_pair_scores=True, sims_for_mixed_pairs=[.2],
                                     remove_boundary_items=False, remove_boundary_affils=False,
                                     method_spec=methods, prefer_faiss=True)

    
