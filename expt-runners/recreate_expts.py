
import numpy as np
from timeit import default_timer as timer
import sys
sys.path.append("../python-scoring")

import experiment_runner
import scoring_methods

def synthetic_expts(base_dir, stored_pi_vector_file_gz):
    # read pi_vectors
    pi_vectors = np.loadtxt(stored_pi_vector_file_gz, delimiter=",")

    methods = set(scoring_methods.all_defined_methods) - {'weighted_corr_exp'} # no exponential model

    # for each of the 69 settings, construct its name, then call score_whole_directory on it
    for setting_num in range(1, 70):
        start = timer()
        current_pi_vector = pi_vectors[setting_num - 1]
        data_dir = base_dir + "/pi" + str(setting_num) + "t0.2"
        experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_sept5_nofaiss", pi_vector_to_use=current_pi_vector,
                                                run_in_parallel=True, remove_boundary_items=False,
                                                sims_for_mixed_pairs=[.2], method_spec=methods, save_pair_scores=False, prefer_faiss=False)
        end = timer()
        print("Ran " + data_dir + " in " + str(end - start) + " secs\n\n\n")


def record_of_runs():

    stored_pi_vector_file_gz = '/home/lfriedl/bipartite-pairs-expts/data-prep/synth/pi_vectors.csv.gz'
    base_dir = '/home/lfriedl/ASOUND-bipartite/expts/synth/new'
    synthetic_expts(base_dir, stored_pi_vector_file_gz)

    base_dir = '/home/lfriedl/ASOUND-bipartite/expts/reality-mining/new'
    real_data_reality(base_dir)

    base_dir = '/home/lfriedl/ASOUND-bipartite/expts/congress/new'
    real_data_congress(base_dir)    # in inf_sept17, using remove_boundary_items=False to match old results from R
    real_data_congress(base_dir)    # in inf_sept25, using remove_boundary_items=True so exp model can be fit

    base_dir = '/home/lfriedl/ASOUND-bipartite/expts/newsgroups/new'
    real_data_newsgroups(base_dir)


def real_data_reality(base_dir):
    settings = ['allPairs-appsByDay', 'allPairs-appsByWeek', 'allPairs-bluetoothByDay', 'allPairs-bluetoothByWeek',
                'allPairs-cellTowersByDay', 'allPairs-cellTowersByWeek']
    tHats = [.001, .005, .01, .03, .05, .07, .09]
    for dir in settings:
        start = timer()
        data_dir = base_dir + "/" + dir
        experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_sept17",
                                                run_in_parallel=True, remove_boundary_items=False,
                                                save_pair_scores=False, sims_for_mixed_pairs=tHats, prefer_faiss=True)
        end = timer()
        print("Ran " + data_dir + " in " + str(end - start) + " secs\n\n\n")


def real_data_congress(base_dir):
    settings = ['dem110', 'dem111', 'dem112', 'dem113', 'rep110', 'rep111', 'rep112', 'rep113']
    tHats = [.001, .01, .05, .1, .4, .5]
    for dir in settings:
        start = timer()
        data_dir = base_dir + "/" + dir
        experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_sept17",
                                                run_in_parallel=True, remove_boundary_items=False,
                                                save_pair_scores=False, sims_for_mixed_pairs=tHats, prefer_faiss=True)
        end = timer()
        print("Ran " + data_dir + " in " + str(end - start) + " secs\n\n\n")


def real_data_newsgroups(base_dir):
    settings = ['alt.atheism', 'comp.graphics', 'comp.os.ms-windows.misc', 'comp.sys.ibm.pc.hardware',
                'comp.sys.mac.hardware', 'comp.windows.x', 'misc.forsale', 'rec.autos', 'rec.motorcycles',
                'rec.sport.baseball', 'rec.sport.hockey', 'sci.crypt', 'sci.electronics', 'sci.med', 'sci.space',
                'soc.religion.christian', 'talk.politics.guns', 'talk.politics.mideast', 'talk.politics.misc',
                'talk.religion.misc']
    tHats = [.001, .005, .01, .02, .03, .04, .05, .06]
    for dir in settings:
        start = timer()
        data_dir = base_dir + "/" + dir
        experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_sept25",
                                                run_in_parallel=True, remove_boundary_items=False,
                                                save_pair_scores=False, sims_for_mixed_pairs=tHats, prefer_faiss=True,
                                                affil_subset_every_1_4=True)
        end = timer()
        print("Ran " + data_dir + " in " + str(end - start) + " secs\n\n\n")


def data_for_test_cases():
    # the trial that crashed with an all-zero item. (used as basis for test cases.)
    base_dir = '/home/lfriedl/ASOUND-bipartite/expts/reality-mining/new'
    data_dir = base_dir + "/allPairs-appsByDay"
    experiment_runner.score_data_set(data_dir, 94, inference_dir_name="inf_sept6", remove_boundary_items=True)

    methods = set(scoring_methods.all_defined_methods) - {'weighted_corr_exp'}
    # note: need to turn off faiss to make numbers match, because it causes .001 diff in auc for weighted_corr.
    experiment_runner.score_data_set(data_dir, 94, inference_dir_name="inf_sept10", remove_boundary_items=False,
                                     method_spec=methods, prefer_faiss=False)


if __name__ == "__main__":
    base_dir = '/home/lfriedl/ASOUND-bipartite/expts/newsgroups/new'
    data_dir = base_dir + '/alt.atheism'
    experiment_runner.score_data_set(data_dir, 1, inference_dir_name="inf_sept25", remove_boundary_items=False,
                                     affil_subset_every_1_4=True)
    # real_data_newsgroups(base_dir)
    exit(0)

    # trial runs for synthetic
    stored_pi_vector_file_gz = '/home/lfriedl/bipartite-pairs-expts/data-prep/synth/pi_vectors.csv.gz'
    base_dir = '/home/lfriedl/ASOUND-bipartite/expts/synth/new'

    pi_vectors = np.loadtxt(stored_pi_vector_file_gz, delimiter=",")
    # while testing, just do a limited set
    # for setting_num in range(8, 15):
    #     current_pi_vector = pi_vectors[setting_num - 1]
    #     data_dir = base_dir + "/pi" + str(setting_num) + "t0.2"
    #     experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_test",
    #                                             pi_vector_to_use=current_pi_vector, num_trials=12, run_in_parallel=True)
    #     experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_test_no_pi",
    #                                             num_trials=12, run_in_parallel=True)

    # check handling of all-0 items: these two versions should come out different (and exponential will have NAs)
    methods = set(scoring_methods.all_defined_methods) - {'weighted_corr_exp'}
    for setting_num in range(60, 62):
    #for setting_num in range(1, 2):
        current_pi_vector = pi_vectors[setting_num - 1]
        data_dir = base_dir + "/pi" + str(setting_num) + "t0.2"
        experiment_runner.score_whole_directory(data_dir, num_trials=20, inference_dir_name="inf_test", pi_vector_to_use=current_pi_vector,
                                                run_in_parallel=True, remove_boundary_items=False, remove_boundary_affils=False,
                                                sims_for_mixed_pairs=[.2], method_spec=methods, save_pair_scores=False, prefer_faiss=False)
        #experiment_runner.score_data_set(data_dir, 154, inference_dir_name="inf_keep_all_items",
        #                                        pi_vector_to_use=current_pi_vector,
        #                                        remove_boundary_items=False, method_spec=methods, prefer_faiss=False)
        # experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_keep_all_items",
        #                                         pi_vector_to_use=current_pi_vector, num_trials=1, run_in_parallel=False,
        #                                         remove_boundary_items=False, method_spec=methods)
        #experiment_runner.score_whole_directory(data_dir, inference_dir_name="inf_rm_all0",
        #                                        pi_vector_to_use=current_pi_vector, num_trials=20, run_in_parallel=True,
        #                                        remove_boundary_items=True)

    # np.nan_to_num([-np.inf, 0, 1, np.nan, np.inf])