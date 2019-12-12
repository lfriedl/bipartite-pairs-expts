# Overview of data preparation

Each data set has different format for its raw and intermediate files.

Broadly, they each go through a process like this:

1. Raw --> Preprocessed1 --> define what the "true pairs" are
* Raw --> Preprocessed2 --> create bipartite graph
* Using bipartite graph + true pairs --> create samples to use for experiments.

In the experimental setup I've been using, each sample is a miniature bipartite graph, a subgraph of the original one. It contains some (few) true pairs and some (many) singletons, along with the affiliations that those items have. The experiment code reads one sample and computes scores for all its pairs of items.
     
(The samples are stored in a consistent format. At least for most data sets...)

(I've also been considering whether to change the experimental setup to sample positive and negative pairs directly from the whole data set's bipartite graph. The biggest bottleneck would be adapting the scoring code to work on an arbitrary set of pairs. It's been on the todo list forever, but isn't implemented yet.)


# Newsgroups

Source data: <http://qwone.com/~jason/20Newsgroups/20news-19997.tar.gz>. Consists of 20 separate groups, and (almost) 1000 posts per group. (Total of 19,997 posts.)

Preprocessing of raw docs: removed headers, split on whitespace, removed punctuation, made lower case. 

True pairs: 

* Defined as posts that share a block of text of at least 10 words. 
* Number of true pairs per group ranges from 385 (in `misc.forsale.pairsGE10`) to 13,429 (in `talk.politics.mideast.pairsGE10`).
* [Data in this repo](https://github.com/lfriedl/bipartite-pairs-expts/tree/master/data-prep/newsgroups-define_true_pairs/20_newsgroup.pairs): a file for each newsgroup lists the IDs of articles that are pairs, plus the length of their longest shared block of text. Constructed by [this code](https://github.com/lfriedl/bipartite-pairs-expts/blob/master/data-prep/define_true_pairs/newsgroups/all-commands.R).

Bipartite graph (one matrix per newsgroup): each row is a document, each column is a word in the vocabulary. Matrix entries are either 0 or 1.

To sample graphs for expts: sample 5 disjoint positive pairs, then 65 singletons, to make a total of 75 items. Makes sure that the sample doesn't (accidentally) include any additional true pairs. (Code in  [prepSamplesRealData.R](https://github.com/lfriedl/bipartite-pairs-expts/blob/master/data-prep/sample_graphs/prepSamplesRealData.R):`createRealDataForNewsgroup()`)

# Reality mining

Source data: <http://realitycommons.media.mit.edu/realitymining.html> (must be requested). Specifically, the Matlab file `realitymining.mat`. This contains (among other things), for each person, the timestamped (or daily) records of each phone app they used, each bluetooth device detected, and each cell phone tower detected.

Preprocessing: events extracted into text files (with minimal filtering) and aggregated into daily and weekly summaries per person. [Code](https://github.com/lfriedl/bipartite-pairs-expts/tree/master/data-prep/reality_mining-preproc).

True pairs: instances of the same person on different days (or weeks, respectively).

Bipartite graph: each row is a person + day (or week), each column is an app name (or the ID of a bluetooth device or cell tower). Code in [dataReader.R](https://github.com/lfriedl/bipartite-pairs-expts/blob/master/data-prep/sample_graphs/dataReader.R):`readAllRealityData()`.

Sampling: choose some person IDs to be in pairs and others to be singletons. Then for each singleton, choose a day/week to use (among those seen in the data), and for each pair, choose two days/weeks. Code in [dataReader.R](https://github.com/lfriedl/bipartite-pairs-expts/blob/master/data-prep/sample_graphs/dataReader.R):`sampleRealityMiningDataFromMatrix()`.

