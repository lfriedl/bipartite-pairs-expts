
# changed from comboPlot.R after supporting code had gone through more revisions

flippedMethods = "A"			#  F means don't, T means flip the methods that benefit, and "A" means flip them all
flippedMSponsLabels = F
noButFlipOnlyCongress = F		# special -- see just below

if (noButFlipOnlyCongress) {	# no longer doing flipping on method by method basis. Rather, leave NG and R unflipped, and flip Congress.
	flippedMethods = F
	pdf("3plotsFlipCongress-noflipSpons2.pdf", width=7, height=9)
} else if (flippedMethods == F) {
	if (flippedMSponsLabels) {
		pdf("3plotsNoFlips-flipSpons2.pdf",  width=7, height=9)
	} else {
		pdf("3plotsNoFlips-noflipSpons2.pdf", width=7, height=9)
	}
} else if (flippedMethods == T) {
	if (flippedMSponsLabels) {
		pdf("3plotsFlippedSome-flipSpons2.pdf", width=7, height=9)
	} else {
		pdf("3plotsFlippedSome-noflipSpons2.pdf", width=7, height=9)
	}
} else {	# flippedMethods = "A"
	if (flippedMSponsLabels) {
		pdf("3plotsFlippedAll-flipSpons2.pdf", width=7, height=9)
	} else {
		pdf("3plotsFlippedAll-noflipSpons2.pdf", width=7, height=9)
	}
}


# old:
# Want to copy/paste results of other functions, and arrange in a figure that looks like:
# | newsgroups     | legend |
# | reality  | congress     |

# new: for thesis, arrange figures as
# | newsgroups 				   | 
# | reality  | legend (2 rows) |
# | congress |				   |
topBottomSplit = split.screen(figs=matrix(c(0, 1, .65, 1, 0, 1, 0, .65), ncol=4, byrow=T))   # split the screen 1/3-way down
						# (syntax: each matrix row is left, right, bottom, top coords of a screen)
upperLeftSplit =  split.screen(figs=matrix(c(0, .9, 0, 1, .9, 1, 0, 1), ncol=4, byrow=T), screen=topBottomSplit[1])	# split upper screen to leave room on the right
leftRightSplit = split.screen(figs=matrix(c(0, .6, 0, 1, .6, 1, 0, 1), ncol=4, byrow=T), screen=topBottomSplit[2])	# split the bottom screen .6 of the way to the right
bottomFigScreens = split.screen(figs=matrix(c(0, 1, .5, 1, 0, 1, 0, .5), ncol=4, byrow=T), screen=leftRightSplit[1])	# further split the bottom left screen 50/50 top/bottom

# plot NG
screen(upperLeftSplit[1])
#source("plotNG_Reality.R")
source("plotNG_Reality3.R")
paramsListForLegend = plotNGReality(flipped = flippedMethods)

# plot legend
screen(leftRightSplit[2])
legend("center", legend=paramsListForLegend$names, lty=paramsListForLegend$lty, col=paramsListForLegend$cols, 
		pch=paramsListForLegend$pch, cex=.85, 
		pt.cex=1.5,		# don't shrink the points as much as the text
		#y.intersp=.6, 	# reduce space between lines
		#seg.len=1.5, 		# shorter line segments
		bty="n")		# no box around legend

# plot reality
screen(bottomFigScreens[1])
plotNGReality(realityMining = T, flipped = flippedMethods)

screen(bottomFigScreens[2])
# plot congress
#source("congressPlot.R")	# do this exactly when needed, because helper functions differ between files (yet have same name) 
#plotCongress(flipVotes=flippedMethods, flipSpons=flippedMSponsLabels)
source("/Users/lfriedl/Documents/dissertation/binary-ndim/congress/prettierDataPlots.R")	
if (flippedMethods == T || noButFlipOnlyCongress) {
	plotCongressManyTrials(flipVotes="A", forTallNarrowCombo=T)
} else {
	plotCongressManyTrials(flipVotes=flippedMethods, forTallNarrowCombo=T)
}

close.screen(all.screens=T)
dev.off()
