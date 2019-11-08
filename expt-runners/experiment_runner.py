import os
import re
import subprocess
import sys
import tarfile
import warnings
from functools import partial
import multiprocessing
from scipy import sparse

sys.path.append("../python-scoring")
sys.path.append("../expt-code")
#sys.path.append("/home/lfriedl/bipartite-pairs/python-scoring")  # (make a proper __init.py__ later?)
import expts_labeled_data
import score_data
import prelim_loc_data_expts


def score_data_set(data_dir, trial_num, inference_dir_name, method_spec="all",
                   save_pair_scores=True, sims_for_mixed_pairs=None, flip_high_ps=False,
                   remove_boundary_items=False, remove_boundary_affils=True, pi_vector_to_use=None, prefer_faiss=True,
                   affil_subset_every_1_4=False, loc_data_bipartite_file=None, loc_data_true_pairs_file=None,
                   verbose=False):
    """
    This function hard-codes some file names (to standardize):
        data_dir/allInputDataFiles.tgz contains files of the form data1_adjMat.mtx.gz, data1_numPos.txt and data1_phi.txt.gz
        output: data_dir/inference_dir_name (created if needed) will have results1.txt and (maybe) scoredPairs1.csv.gz

    :param data_dir:
    :param trial_num: integer
    :param inference_dir_name: subdirectory will be constructed (if necessary) under data_dir
    :param method_spec:
    :param save_pair_scores: whether to write a scoredPairs file
    :param sims_for_mixed_pairs: default runs a standard set of 3
    :param flip_high_ps:
    :param remove_boundary_items:
    :param remove_boundary_affils:
    :param pi_vector_to_use:
    :param prefer_faiss:
    :param affil_subset_every_1_4: to match existing handling of newsgroups data, keep only the 1st of every 4 affils
        (pruned before handing to score_data package).
    :return:
    """

    # 1a. construct input filenames and extract data file(s) from tar archive, if not already found locally.
    loc_data_format = (loc_data_bipartite_file is not None and loc_data_true_pairs_file is not None)
    if loc_data_format:
        basenamefiles = ["./data" + str(trial_num) + ".rowIDs"]
    else:   # usual format
        # what we need are adjMat and numPos.
        adj_mat_file_basename = "./data" + str(trial_num) + "_adjMat.mtx.gz"    # nb: "./" required to access it from tar
        num_pos_file_basename = "./data" + str(trial_num) + "_numPos.txt"
        adj_mat_infile = data_dir + "/" + adj_mat_file_basename
        num_pos_infile = data_dir + "/" + num_pos_file_basename
        basenamefiles = [adj_mat_file_basename, num_pos_file_basename]

    fullpathfiles = [data_dir + "/" + basefile for basefile in basenamefiles]
    files_already_present = True

    if not all([os.path.isfile(file) for file in fullpathfiles]):
    # if not (os.path.isfile(adj_mat_infile) and os.path.isfile(num_pos_infile)):
        files_already_present = False
        tar_infile = data_dir + "/allInputDataFiles.tgz"
        tf = tarfile.open(tar_infile)
        members = [tf.getmember(filename) for filename in basenamefiles]
        # members = [tf.getmember(filename) for filename in [adj_mat_file_basename, num_pos_file_basename]]
        tf.extractall(path=data_dir, members=members)


    # 1b. construct other variables

    # outfiles
    # create or locate outputDir
    out_dir = data_dir + "/" + inference_dir_name
    if not (os.path.isdir(out_dir)):
        try:
            os.mkdir(out_dir)
        except OSError:
            # test again, because another thread might have created it meanwhile
            ok = os.path.isdir(out_dir)
            if not ok:
                os.mkdir(out_dir)   # an error here stops the function

    # (full paths)
    evals_outfile = out_dir + "/results" + str(trial_num) + ".txt"
    if save_pair_scores:
        pair_scores_outfile = out_dir + "/scoredPairs" + str(trial_num) + ".csv.gz"
    else:
        pair_scores_outfile = None
    if sims_for_mixed_pairs is None:
        sims_for_mixed_pairs = "standard"   # magic word for default

    # load data into variables
    if loc_data_format:
        adj_mat, item_names, true_labels_func = prelim_loc_data_expts.get_loc_expt_data(loc_data_bipartite_file,
                                                                                       loc_data_true_pairs_file,
                                                                                       fullpathfiles[0])
    else:
        adj_mat = score_data.load_adj_mat(adj_mat_infile)
        item_names = None
        with open(num_pos_infile) as fin:
            num_true_pos = int(fin.readline())
        true_labels_func = partial(expts_labeled_data.get_true_labels_expt_data, num_true_pos)

    if affil_subset_every_1_4:
        subset_adj_mat = sparse.csc_matrix((adj_mat.shape[0], int(adj_mat.shape[1]/4)), dtype='int')
        # copy over every 4th row
        for i in range(int(adj_mat.shape[1]/4)):
            subset_adj_mat[:, i] = adj_mat[:, 4 * i]
        adj_mat = subset_adj_mat
        if pi_vector_to_use is not None:
            subset_pi_vector = [pi_vector_to_use[4*i] for i in range(int(len(pi_vector_to_use)/4))]
            pi_vector_to_use = subset_pi_vector

    if not files_already_present:
        [os.remove(infile) for infile in fullpathfiles]

    # 2. run the expt
    score_data.run_and_eval(adj_mat, true_labels_func=true_labels_func,
                            method_spec=method_spec,
                            evals_outfile=evals_outfile,
                            pair_scores_outfile=pair_scores_outfile, row_labels=item_names,
                            print_timing=verbose, prefer_faiss=prefer_faiss,
                            mixed_pairs_sims=sims_for_mixed_pairs,
                            flip_high_ps=flip_high_ps, pi_vector_to_use=pi_vector_to_use,
                            remove_boundary_items=remove_boundary_items, remove_boundary_affils=remove_boundary_affils)


# note: when adding new params, remember to modify plain AND parallel calls.
def score_whole_directory(data_dir, inference_dir_name, method_spec="all",
                          num_trials=None, extract_all=True, run_in_parallel=False,
                          save_pair_scores=True, sims_for_mixed_pairs=None, flip_high_ps=False,
                          remove_boundary_items=False, remove_boundary_affils=True,
                          pi_vector_to_use=None, prefer_faiss=True, affil_subset_every_1_4=False,
                          loc_data_bipartite_file=None, loc_data_true_pairs_file=None,
                          verbose=False):
    """
    Runs all trials in a directory.
    :param data_dir:
    :param inference_dir_name:
    :param num_trials: runs on data sets 1 to num_trials. defaults to the max value seen.
    :param extract_all: if false, make each worker function extract just the data it needs (the default, extracting
                        all at once, is faster but messier)
    :param run_in_parallel: use multiprocessing package (pool of 10 workers) -- faster, with a messy stdout
    :param save_pair_scores:
    :param sims_for_mixed_pairs: "None" means "use the default set"
    :param flip_high_ps:
    :return:
    """

    print("Running experiments in " + data_dir)

    # look at input files
    tar_infile = data_dir + "/allInputDataFiles.tgz"
    with tarfile.open(tar_infile) as tf:
        tar_extracted_files = tf.getnames()
        if extract_all:
            tf.extractall(path=data_dir)

    if num_trials is None:
        # get num_trials from the number of data files
        num_trials = 0
        regexp = re.compile("data([0-9]+)")
        for filename in tar_extracted_files:
            m = regexp.search(filename)
            if m is not None:
                num = int(m.group(1))
                if num > num_trials:
                    num_trials = num
        print("going to run on the " + str(num_trials) + " data sets found")
    else:
        print("going to run on the first " + str(num_trials) + " data sets")


    # run the experiments
    if run_in_parallel:
        print("About to run the trials in parallel")
        pool = multiprocessing.Pool(processes=10)
        mp_results = [pool.apply_async(func=score_data_set,
                              args=[data_dir, trial, inference_dir_name],
                              kwds={'save_pair_scores': save_pair_scores,
                                    'sims_for_mixed_pairs': sims_for_mixed_pairs, 'flip_high_ps': flip_high_ps,
                                    'pi_vector_to_use': pi_vector_to_use, 'method_spec': method_spec,
                                    'remove_boundary_items': remove_boundary_items,
                                    'remove_boundary_affils': remove_boundary_affils,
                                    'prefer_faiss': prefer_faiss,
                                    'affil_subset_every_1_4': affil_subset_every_1_4,
                                    'loc_data_bipartite_file': loc_data_bipartite_file,
                                    'loc_data_true_pairs_file': loc_data_true_pairs_file,
                                    'verbose': verbose})
             for trial in range(1, num_trials+1)]
        [r.get() for r in mp_results]   # r.get() blocks until process completes
        pool.close()
        pool.join()

    else:
        for trial in range(1, num_trials+1):
            score_data_set(data_dir, trial, inference_dir_name,
                           method_spec=method_spec, save_pair_scores=save_pair_scores,
                           sims_for_mixed_pairs=sims_for_mixed_pairs, flip_high_ps=flip_high_ps,
                           remove_boundary_items=remove_boundary_items, remove_boundary_affils=remove_boundary_affils,
                           pi_vector_to_use=pi_vector_to_use, prefer_faiss=prefer_faiss,
                           affil_subset_every_1_4=affil_subset_every_1_4,
                           loc_data_bipartite_file=loc_data_bipartite_file,
                           loc_data_true_pairs_file=loc_data_true_pairs_file, verbose=verbose)


    # clean up and summarize
    if extract_all:
        # delete extracted input files
        [os.remove(data_dir + "/" + infile) for infile in tar_extracted_files if (os.path.isfile(data_dir + "/" + infile))]

    # Try to avoid clobbering data or hiding errors: only tar and remove results files if we find exactly the right
    # number of trials.
    #  too few files --> error
    #  too many results files --> warning, stops midway
    #  too many scored pairs files --> silent, extra ones stay there
    num_results_found = summarize_results(data_dir + "/" + inference_dir_name, num_trials)

    if save_pair_scores and num_results_found == num_trials:
        # tar up scored-pairs files
        tar_spfile = data_dir + "/" + inference_dir_name + "/allScoredPairsFiles.tar"
        with tarfile.open(tar_spfile, mode='w:') as tf:
            for trial_num in range(1, num_trials + 1):
                local_filename = "scoredPairs" + str(trial_num) + ".csv.gz"
                tf.add(data_dir + "/" + inference_dir_name + "/" + local_filename, arcname=local_filename)
        # delete them if that worked
        for trial_num in range(1, num_trials + 1):
            local_filename = "scoredPairs" + str(trial_num) + ".csv.gz"
            os.remove(data_dir + "/" + inference_dir_name + "/" + local_filename)



def summarize_results(inference_dir_path, num_trials_expected):
    """
    Creates avgs.txt and vars.txt using R script, and leaves a file numTrials.txt to go with them.
    Aborts if the directory contains an unexpected number of results files.
    Then tars up the results files.
    :param inference_dir_path:
    :return: number of results files found
    """
    # run retrieveResultsCI.R, which is in same directory as this file
    path_to_R_script = os.path.dirname(os.path.abspath(__file__)) + '/retrieveResultsCI.R'
    sys_cmd_to_run = "Rscript -e source('" + path_to_R_script + "') -e computeAvgsVars(directory='" + \
                     inference_dir_path + "')"
    subprocess.check_call(sys_cmd_to_run.split(' '))    # raises an error if return code != 0

    # record the number of results files we found and check it's right
    path_files = os.listdir(inference_dir_path)
    regexp = re.compile("results[0-9]+\.txt$")
    res_files = [x for x in path_files if regexp.match(x)]
    num_results_found = len(res_files)
    with open(inference_dir_path + "/numTrials.txt", 'w') as fp:
        fp.write(str(num_results_found) + "\n")

    if num_results_found != num_trials_expected:
        warnings.warn("ran expt on " + str(num_trials_expected) + " data files, " + \
                             "but found " + str(num_results_found) + " results files")
        return num_results_found

    # tar up results files
    tar_resfile = inference_dir_path + "/allResultsFiles.tgz"
    with tarfile.open(tar_resfile, mode='w:gz') as tf:
        [tf.add(inference_dir_path + "/" + res_file, arcname=res_file) for res_file in sorted(res_files)]

    # delete results files
    [os.remove(inference_dir_path + "/" + res_file) for res_file in res_files]

    return num_results_found


if __name__ == "__main__":
    # newsgroups isn't a good choice to test, b/c most results used quarter affils
    # score_data_set(data_dir='/home/lfriedl/ASOUND-bipartite/expts/newsgroups/new/alt.atheism', trial_num=1,
    #                inference_dir_name='inf_test')

    # score_data_set(data_dir='/home/lfriedl/ASOUND-bipartite/expts/reality-mining/new/allPairs-appsByDay', trial_num=1,
    #                inference_dir_name='inf_test')
        # differs in auc_m(??), auc_idf, auc_cosine, pearson, ... hmm ok, need to look at preprocessing

    # score_whole_directory(data_dir='/home/lfriedl/ASOUND-bipartite/expts/reality-mining/new/allPairs-appsByDay',
    #                       inference_dir_name="inf_test", num_trials=20)
    score_whole_directory(data_dir='/home/lfriedl/ASOUND-bipartite/expts/reality-mining/new/allPairs-appsByDay',
                          inference_dir_name="inf_test", num_trials=20, run_in_parallel=True)