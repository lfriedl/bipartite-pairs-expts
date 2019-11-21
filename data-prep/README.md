# Newsgroups

Source data: <http://qwone.com/~jason/20Newsgroups/20news-19997.tar.gz>

Preprocessing of raw docs: removed headers, split on whitespace, removed punctuation, made lower case.

True pairs: defined as posts that share a block of text of at least 10 words.

Bipartite graph (one matrix per newsgroup): each row is a document, each column is a word in the vocabulary. Matrix entries are either 0 or 1.

To sample graphs for expts: sample 5 positive pairs, then 65 singletons, to make a total of 75 items. Makes sure that the sample doesn't (accidentally) include any additional true pairs. 
