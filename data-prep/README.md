# Overview of data preparation
(This README is for "real data." For synthetic data, see [synthetic_data subfolder](synthetic_data).)

Each data set has different formats for its raw and intermediate files.

Broadly, they each go through a process like this:

1. Raw --> Preprocessed1 --> create bipartite graph
1. Raw --> Preprocessed2 --> define what the "true pairs" are
1. (For some data sets:) Use bipartite graph + true pairs --> create samples to use for experiments. (For other data sets, we skip this step and simply score all pairs of items in the bipartite graph.)

In the experimental setup I've been using, a sample is a subgraph of the bipartite graph. It's formed from a set of (under 100) items plus all their affiliations. The set of items contains some (few) true pairs and some (many) singletons. 

The experiment code then reads one sample (or, for some data sets, the whole bipartite graph) and computes scores for all its pairs of items.
     
The bipartite graphs are stored in Matrix Market format (`.mtx`). Most preprocessing was originally done in R, so a lot of data is initially stored in .Rdata format, then converted to .mtx. The true pairs are either stored as text files with lists of pairs or as additional (sparse) graphs in .mtx format. Matrix Market format doesn't include node names, so we store these in additional files with the suffixes `.itemnames` and `.affilnames`.



# Newsgroups

Source data: <http://qwone.com/~jason/20Newsgroups/20news-19997.tar.gz>. Consists of 20 separate groups, and (almost) 1000 posts per group. (Total of 19,997 posts.)

Preprocessing of raw docs: removed headers, split on whitespace, removed punctuation, made lower case. 

True pairs: 

* Defined as posts that share a block of text of at least 10 words. 
* Number of true pairs per group ranges from 385 (in `misc.forsale.pairsGE10`) to 13,429 (in `talk.politics.mideast.pairsGE10`).
* [Data in this repo](newsgroups-define_true_pairs/20_newsgroup.pairs): a file for each newsgroup lists the IDs of articles that are pairs, plus the length of their longest shared block of text. Constructed by [this code](newsgroups-define_true_pairs/all-commands.R).

Bipartite graph (one matrix per newsgroup): each row is a document, each column is a word in the vocabulary. Matrix entries are either 0 or 1.

To sample graphs for expts: sample 5 disjoint positive pairs, then 65 singletons, to make a total of 75 items. Makes sure that the sample doesn't (accidentally) include any additional true pairs. (Code in  [prepSamplesRealData.R](sample_graphs/prepSamplesRealData.R):`createRealDataForNewsgroup()` creates samples. The .Rdata format is then converted to .mtx format using calls to [converting-from-R/prepExptDirWithTar.R](converting-from-R/prepExptDirWithTar.R):`convertDataDirToTar()`.)

# Reality mining

Source data: <http://realitycommons.media.mit.edu/realitymining.html> (must be requested). Specifically, the Matlab file `realitymining.mat`. This contains (among other things), for each person, the timestamped (or daily) records of each phone app they used, each bluetooth device detected, and each cell phone tower detected.

Preprocessing: events extracted into text files (with minimal filtering) and aggregated into daily and weekly summaries per person. [Code in `reality_mining`](reality_mining) (see README there).

True pairs: instances of the same person on different days (or weeks, respectively).

Code that creates/saves bipartite graph + samples small graphs: top-level call is [`reality_mining/realitySampleGraphs.R`](reality_mining/realitySampleGraphs.R).

* Bipartite graph: each row is a person + day (or week), each column is an app name (or the ID of a bluetooth device or cell tower). Code in [dataReader.R](sample_graphs/dataReader.R):`readAllRealityData()`.

* Sampling: choose some person IDs to be in pairs and others to be singletons. Then for each singleton, choose a day/week to use (among those seen in the data), and for each pair, choose two days/weeks. Code in [dataReader.R](sample_graphs/dataReader.R):`sampleRealityMiningDataFromMatrix()` both creates samples and converts them from .Rdata to .mtx.

# Congress

Source data: U.S. House of Representatives legislators, bill sponsorships, and votes. Data obtained from [congress-legislators](https://www.github.com/unitedstates/congress-legislators) and [congress](https://github.com/unitedstates/congress) repositories maintained by <https://theunitedstates.io/>; see [congress/raw-data/README.md](congress/raw-data/README.md) for details.

Preprocessing and bipartite graphs: Run `getAllMatrices.R` in [`create-adj-matrices`](create-adj-matrices). Given a Congress number, get list of legislators in office with their parties. Create adjacency matrices in which rows are legislators of one party and ...

* Bill sponsorship matrix: each column is a bill introduced (of type H.R. or H.J.Res.), with 1s for the bill's sponsors and cosponsors (ignoring any cosponsors that later withdrew support). 

* Votes matrix: each column is a roll call vote which was not unanimous and for which there was a yes option (either "Yea" or "Aye"). Yes gets a 1 in the matrix.

* Matrix sizes (for Congresses 110 to 113): 183 to 246 members of Congress in each party, 5511 to 7439 bills sponsored, 1199 to 1864 roll call votes.

True pairs: Pairs of legislators who had the highest number of cosponsorships. Uses the bill sponsorship matrix. For each party x session, a different cutoff on cosponsorships is picked, to select < 1% of all pairs in that data set. (Code in [congress/true-pairs/createTruePairsFiles.R](congress/true-pairs/createTruePairsFiles.R):`runAll()`.)


Sampling: none (any more). Scoring all pairs in the bipartite matrix. The two functions in [congress/create-adj-matrices/convertMatrices.R](congress/create-adj-matrices/convertMatrices.R) create .mtx files for:

* the bipartite matrix
* the cosponsorship matrix
* the true pairs (a thresholded binary version of the cosponsorship matrix).

