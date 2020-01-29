
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


def record_of_runs():

    stored_pi_vector_file_gz = '/home/lfriedl/bipartite-pairs-expts/data-prep/synthetic_data/params/pi_vectors.csv.gz'
    base_dir = '/home/lfriedl/ASOUND-bipartite/expts/synth/new1000'
    synthetic_expts(base_dir, stored_pi_vector_file_gz)


if __name__ == "__main__":
    stored_pi_vector_file_gz = '/home/lfriedl/bipartite-pairs-expts/data-prep/synthetic_data/params/pi_vectors.csv.gz'
    base_dir = '/home/lfriedl/ASOUND-bipartite/expts/synth/new1000'
    synthetic_expts(base_dir, stored_pi_vector_file_gz)
    exit(0)

	# testing/debugging
    pi_vectors = np.loadtxt(stored_pi_vector_file_gz, delimiter=",")
    methods = set(scoring_methods.all_defined_methods) - {'weighted_corr_exp'} # no exponential model
    setting_num = 1
    current_pi_vector = pi_vectors[setting_num - 1]
    data_dir = base_dir + "/pi" + str(setting_num) + "t0.2"
    experiment_runner.score_data_set(data_dir, 1, inference_dir_name="test_jan28", pi_vector_to_use=current_pi_vector, 
            verbose=True, save_pair_scores=True, sims_for_mixed_pairs=[.2], remove_boundary_items=False, method_spec=methods, prefer_faiss=False)

    
