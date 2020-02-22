import numpy as np
import experiment_runner
import sys
sys.path.append("../expt-code")
import score_data


# translated from R code
def compute_affil_subsets(pi_vector, affil_subset_fraction, affil_subset_type = 1):
    """

    :param pi_vector: column means of adj matrix, unless provided from an oracle
    :param affil_subset_fraction: in (0,1]
    :param affil_subset_type:
            1 = random p_i
            2 = "max" p_i: those closest to .5. Should give best AUCs.
            3 = "min" p_i: most extreme values. Should give worst AUCs.
    :return: boolean vector the length of pi_vector
    """

    num_to_keep = int(np.floor(affil_subset_fraction * len(pi_vector)))
    print("affil_subset_fraction of " + str(affil_subset_fraction) + " yields a size of " +
                str(num_to_keep))

    print("Constructing affilsToKeep for this run")
    affils_to_keep = np.full(shape=len(pi_vector), fill_value=False)

    if affil_subset_type == 1:
        # randomly sample
        indicesWanted = np.random.choice(len(pi_vector), size=num_to_keep, replace=False)
        affils_to_keep[indicesWanted] = True
    elif affil_subset_type == 2:
        distFromPt5 = np.abs(pi_vector - .5)
        # want those closest to .5
        indicesSorted = np.arange(len(pi_vector))[np.argsort(distFromPt5)]

        # need to handle ties right:
        # -find the cutoff element
        # -keep everything below that
        # -sample among the ties at that value (if any)
        cutoffValue = distFromPt5[indicesSorted[num_to_keep]]
        affils_to_keep[distFromPt5 < cutoffValue] = True
        # ties: defined as equal to cutoff or some small epsilon above
        ties = np.logical_and((distFromPt5 >= cutoffValue), (distFromPt5 < cutoffValue + .000001))
        numTies = sum(ties)

        whichTiesToUse = np.random.choice(numTies, num_to_keep - np.sum(affils_to_keep), replace=False)
        # map whichTiesToUse, which holds indices in 1:numTies, into ties vector
        # np.nonzero(ties): short vector containing indices of the ties
        indicesOfTiesToUse = np.nonzero(ties)[0][whichTiesToUse]
        affils_to_keep[indicesOfTiesToUse] = True


    elif affil_subset_type == 3:
        distFromPt5 = np.abs(pi_vector - .5)
        # want those farthest from .5
        indicesSorted = np.arange(len(pi_vector))[np.argsort(distFromPt5)[::-1]]

        # like case 2, but for > & < signs
        cutoffValue = distFromPt5[indicesSorted[num_to_keep]]
        affils_to_keep[distFromPt5 > cutoffValue] = True
        ties = np.logical_and((distFromPt5 <= cutoffValue), (distFromPt5 > cutoffValue - .000001))

        numTies = sum(ties)
        whichTiesToUse = np.random.choice(numTies, num_to_keep - np.sum(affils_to_keep), replace=False)
        indicesOfTiesToUse = np.nonzero(ties)[0][whichTiesToUse]
        affils_to_keep[indicesOfTiesToUse] = True

    return affils_to_keep



# sort of a hack for cases where the adjacency matrix is blank, and we want to assign AUC of 0.5 to all scores.
def print_results_for_0data(num_true_pos, adj_mat, true_labels_func, method_spec, evals_outfile,
                 pair_scores_outfile=None, mixed_pairs_sims='standard', add_exp_model=False,
                 make_dense=True, prefer_faiss=False, print_timing=False,
                 row_labels=None, pi_vector_to_use=None,
                 flip_high_ps=False, remove_boundary_items=True, remove_boundary_affils=True):
    pi_vector, adj_mat, row_labels = score_data.remove_boundary_nodes(adj_mat, pi_vector=pi_vector_to_use,
                                                           flip_high_ps=flip_high_ps, orig_row_labels=row_labels,
                                                           remove_boundary_items=remove_boundary_items,
                                                           remove_boundary_affils=remove_boundary_affils)
    print("No data! Exiting gracefully.")
    if make_dense:
        adj_mat = adj_mat.toarray()

    want_exp_model = add_exp_model or ('weighted_corr_exp' in method_spec) or \
                     ('weighted_corr_exp_faiss' in method_spec) or ('all' in method_spec)
    graph_models = score_data.learn_graph_models(adj_mat, bernoulli=True, pi_vector=pi_vector, exponential=want_exp_model,
                                      verbose=print_timing, max_iter_biment=50000)
    all_defined_methods = ['jaccard', 'cosine', 'cosineIDF', 'shared_size', 'hamming', 'pearson',
                           'shared_weight11', 'shared_weight1100', 'adamic_adar', 'newman', 'mixed_pairs',
                           'weighted_corr', 'weighted_corr_exp']
    if method_spec == 'all':
        method_spec = set(all_defined_methods) - {'weighted_corr_exp'}
    if mixed_pairs_sims == 'standard':
        mixed_pairs_sims = (.1, .01, .001)

    if 'mixed_pairs' in method_spec:
        method_spec.remove('mixed_pairs')
        for mp_sim in mixed_pairs_sims:
            method_spec.add('mixed_pairs_' + str(mp_sim))

    evals = {}
    for method in method_spec:
        evals["auc_" + method] = 0.5

    for model_type, graph_model in list(graph_models.items()):
        (loglik, aic, item_LLs) = graph_model.likelihoods(adj_mat, print_timing=print_timing)
        evals["loglikelihood_" + model_type] = loglik
        evals["akaike_" + model_type] = aic

    evals['constructAllPairsFromMDocs'] = adj_mat.shape[0]      # only correct when we're using all pairs
    evals['numPositives'] = num_true_pos
    evals['numAffils'] = adj_mat.shape[1]
    if want_exp_model:
        evals['expModelConverged'] = int(graph_models['exponential'].exp_model_converged)

    with open(evals_outfile, 'w') as fpout:
        print("Saving results to " + evals_outfile)
        for (measure, val) in sorted(evals.items()):
            fpout.write(measure + '\t' + str(val) + '\n')



def test_affil_subsets():
    # see if we match old results from reality mining
    base_dir = '/home/lfriedl/ASOUND-bipartite/expts/reality-mining/new'
    setting = 'allPairs-appsByDay' #, 'allPairs-appsByWeek', 'allPairs-bluetoothByDay', 'allPairs-bluetoothByWeek',
    #             'allPairs-cellTowersByDay', 'allPairs-cellTowersByWeek']
    data_dir = base_dir + '/' + setting
    # on local machine, that's:
    data_dir = '/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs/tests/reality_appday_134'
    all_methods_to_run = ['jaccard', 'cosine', 'cosineIDF', 'shared_size', 'hamming', 'pearson',
                          'shared_weight11', 'shared_weight1100', 'adamic_adar', 'newman', 'weighted_corr',
                          'mixed_pairs']

    tHat = .01
    trialNum = 134
    experiment_runner.score_data_set(data_dir, trialNum, inference_dir_name="inf_affilsub_1",
                                     remove_boundary_items=False,
                                     affil_subset_type=1, affil_subset_fraction=.25,
                                     save_pair_scores=False, sims_for_mixed_pairs=[tHat], method_spec=all_methods_to_run, prefer_faiss=False)
    experiment_runner.score_data_set(data_dir, trialNum, inference_dir_name="inf_affilsub_2",
                                     remove_boundary_items=False,
                                     affil_subset_type=2, affil_subset_fraction=.25,
                                     save_pair_scores=False, sims_for_mixed_pairs=[tHat], method_spec=all_methods_to_run, prefer_faiss=False)
    experiment_runner.score_data_set(data_dir, trialNum, inference_dir_name="inf_affilsub_3",
                                     remove_boundary_items=False,
                                     affil_subset_type=3, affil_subset_fraction=.25,
                                     save_pair_scores=False, sims_for_mixed_pairs=[tHat], method_spec=all_methods_to_run, prefer_faiss=False)





if __name__ == "__main__":
    test_affil_subsets()
