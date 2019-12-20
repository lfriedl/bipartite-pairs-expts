
library(Matrix)

# Note of caution: a small set of people participate in most of the top links. Make sure data supports a large enough pool 
# that we aren't forced to put the same people in every data sample.
runAll = function() {
	parties = c("dem", "rep")
	sessions = 110:113
	adjMatDir = "../adjacency-matrices"
	truePairsDir = "."
	for (party in parties) {
		for (session in sessions) {
			sponsAdjFile = file.path(adjMatDir, paste0(party, "Cospons", session, ".Rdata"))
			truePairsOutFile = paste0(truePairsDir, "/", party, session, ".txt")
			print(paste("Working on", party, session))
			constructListOfTruePairs(sponsAdjFile, truePairsOutFile, fractionOfPeopleWithALink = 1.5)
		}
	}

}

# Grabbing pairs with high num sponsorships.
# N.B. fractionOfPeopleWithALink dictates how many links we take, but the actual number of people among these links
# will be much smaller. As a result, function works even if fractionOfPeopleWithALink exceeds 1.
constructListOfTruePairs = function(sponsorsAdjMatFile, outFile, fractionOfPeopleWithALink = .25) {
	varsFound = load(sponsorsAdjMatFile)
	sponsMat = get(varsFound)
	cosponsMat = tcrossprod(sponsMat)
	m_s = t(cosponsMat)[lower.tri(cosponsMat)]	# number of shared [co]sponsorships. (trick to get upper.tri but going across rows before cols)

    numLinks = length(m_s)
    numItemsInDataSet = dim(sponsMat)[1]
    numLinksWanted = numItemsInDataSet * fractionOfPeopleWithALink / 2
    percentileWanted = (numLinks - numLinksWanted) / numLinks
    labels = rep(0, length(m_s))
    
    cutoffValue = ceiling(quantile(m_s, probs=percentileWanted))
    labels[m_s >= cutoffValue] = 1

	# who's in these pairs?
	pairNames = expand.grid(colInd = rownames(sponsMat), rowInd = rownames(sponsMat), stringsAsFactors=F)
	# build a similar grid of numbers, to figure out the indices we need
	ijs = expand.grid(1:numItemsInDataSet, 1:numItemsInDataSet)
	ijsWanted = which(ijs$Var2 < ijs$Var1)
	pairNames = pairNames[ijsWanted, 2:1]

	pairsToKeep = pairNames[labels==1, ]
	numDistinctItems = length(unique(c(pairsToKeep[[1]], pairsToKeep[[2]])))
    
	write.table(pairsToKeep, file=outFile, sep="\t", row.names=F, col.names=F, quote=F)

    print(paste("Data set had", numItemsInDataSet, "items; we picked", sum(labels==1), "pairs, which involved",
          numDistinctItems, "distinct items total"))
    print(paste("Min num cosponsorships for a true pair was", cutoffValue, 
                " (", round(100*percentileWanted, 0), "th percentile); max was", max(m_s)))
    
}



# -- (not flipping sponsor) --
# using fractionOfPeopleWithALink = 1.5, got this:
#[1] "Working on dem 110"
#[1] "Data set had 246 items; we picked 186 pairs, which involved 52 distinct items total"
#[1] "Min num cosponsorships for a true pair was 289  ( 99 th percentile); max was 509"
#[1] "Working on dem 111"
#[1] "Data set had 268 items; we picked 202 pairs, which involved 48 distinct items total"
#[1] "Min num cosponsorships for a true pair was 231  ( 99 th percentile); max was 408"
#[1] "Working on dem 112"
#[1] "Data set had 206 items; we picked 160 pairs, which involved 32 distinct items total"
#[1] "Min num cosponsorships for a true pair was 210  ( 99 th percentile); max was 358"
#[1] "Working on dem 113"
#[1] "Data set had 208 items; we picked 156 pairs, which involved 33 distinct items total"
#[1] "Min num cosponsorships for a true pair was 222  ( 99 th percentile); max was 348"
#[1] "Working on rep 110"
#[1] "Data set had 207 items; we picked 162 pairs, which involved 50 distinct items total"
#[1] "Min num cosponsorships for a true pair was 141  ( 99 th percentile); max was 220"
#[1] "Working on rep 111"
#[1] "Data set had 183 items; we picked 138 pairs, which involved 40 distinct items total"
#[1] "Min num cosponsorships for a true pair was 133  ( 99 th percentile); max was 204"
#[1] "Working on rep 112"
#[1] "Data set had 245 items; we picked 194 pairs, which involved 55 distinct items total"
#[1] "Min num cosponsorships for a true pair was 132  ( 99 th percentile); max was 202"
#[1] "Working on rep 113"
#[1] "Data set had 239 items; we picked 191 pairs, which involved 68 distinct items total"
#[1] "Min num cosponsorships for a true pair was 119  ( 99 th percentile); max was 182"
