# something like this . . .

raw_data_dir = "/Users/lfriedl/Documents/dissertation/real-data/20_newsgroup"
all_groups = list.files(raw_data_dir)
out_dir = "20_newsgroup.pairs"

# 1. Each newsgroup has 1000 posts (separate files). For each pair of posts, compute the largest block of text they share
# (largest number of words in a row). Save each newsgroup's pairs in a separate file.
for (ng in all_groups) {
	system(paste("perl longestCommonSubstring.pl", file.path(raw_data_dir, ng), file.path(out_dir, paste0(ng, ".allPairs"))))
}

# 2. After manually determining a cutoff of "10 words or more" to mean a true pair, create a file with just those pairs
for (ng in all_groups) {
	infile = file.path(out_dir, paste0(ng, ".allPairs"))
	outfile = file.path(out_dir, paste0(ng, ".pairsGE10"))
	data = read.table(infile)
	write.table(data[data[,3] >= 10,], file=outfile, row.names=F, col.names=F)
}

