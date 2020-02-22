library(lattice)
source("/Users/lfriedl/Documents/dissertation/binary-ndim/code2/analytics3.R")	
# for speed:
#library(compiler)
#enableJIT(1)  # higher numbers for more aggressive JITting

outdir = "/Users/lfriedl/Documents/dissertation/text/documents/2020\ paper\ 1\ -\ KDD/figures"

plotsForPaper = function(outdir=".") {

	# remove extra space
	theme.nopadding <-
	  list(layout.heights =
         list(top.padding = 0, main.key.padding = 0, key.axis.padding = 0,
              axis.xlab.padding = 0, xlab.key.padding = 0, key.sub.padding = 0,
              bottom.padding = 0),
       layout.widths =
         list(left.padding = 0, key.ylab.padding = 0, ylab.axis.padding = 0,
              axis.key.padding = 0, right.padding = 0))
	trellis.par.set(theme.nopadding)


	#pdf(file.path(outdir, "AUCs_constpi_MixedPairs_k10.pdf"), height=6.5, width=7)
	pdf(file.path(outdir, "AUCs_constpi_MixedPairs_k10.pdf"), height=5, width=5.5)
	trellis.par.set(theme.nopadding)
	createPrettyAUCPlotPiT(10, moreTicks=2)
	dev.off()

	pdf(file.path(outdir, "AUCs_constpi_SharedSize_k100.pdf"), height=5, width=5.5)
	trellis.par.set(theme.nopadding)
	createPrettyAUCPlotPiT(100, scoringModel=11, moreTicks=2)
	dev.off()

	pdf(file.path(outdir, "AUCs_constpi_MixedPairs_k100.pdf"), height=5, width=5.5)
	trellis.par.set(theme.nopadding)
	createPrettyAUCPlotPiT(100, moreTicks=2)
	dev.off()
}

morePlotsMore = function(outDir) {
	scoringMethods = c(rep("undefined", 4), "MixedPairs", "SharedWeight11", "SharedWeight1100", "CosineThrowingOut0s",
						"Cosine", "WeightedCorrelation", "SharedSize", "Hamming", "Adamic", "Jaccard", "Pearson", "CosineRedux")	# (last element = 16)
	#for (n in c(10, 100)) {
	#for (n in c(2, 5)) {
	for (n in c(10, 40)) {
		#for (scoringMethodIndex in c(5, 6, 7, 10, 11, 12)) {		
		for (scoringMethodIndex in c(14, 15, 9)) {
		#for (scoringMethodIndex in c(14, 15)) {
		#for (scoringMethodIndex in c(8)) {
			if (n > 50 && (scoringMethodIndex == 9 || scoringMethodIndex == 14 || scoringMethodIndex == 15)) {
				next	# cosine will take a long time, so skip for now
			}
			plotName = paste0(outDir, "/", "AUCs_constpi_model5_", scoringMethods[scoringMethodIndex], "_n", n, ".pdf")
			print(paste("Working on", plotName))
			pdf(plotName, height=6.5, width=7)
			par(mgp=c(2, 1, 0), mar=c(3, 3, .1, .1))

			theLatticeObject = createPrettyAUCPlotPiT(n=n, scoringModel=scoringMethodIndex, moreTicks=2)
			plot(theLatticeObject)	
			# from lattice manual: 
			# High-level lattice functions like xyplot are different from traditional R graphics functions in that
			# they do not perform any plotting themselves.  Instead, they return an object, of class "trellis",
			# which has to be then print -ed or plot -ted to create the actual plot. Due to R's automatic printing
			# rule, it is usually not necessary to explicitly carry out the second step, and lattice functions appear
			# to behave like their traditional counterparts.  However, the automatic plotting is suppressed when
			# the high-level functions are called inside another function (most often source) or in other contexts
			# where automatic printing is suppressed (e.g., for or while loops).  In such situations, an explicit
			# call to print or plot is required.
			
			dev.off()
		}
	}
}



# Code copied from plots_AUC_fn_npi.R to modify the title and axes labels.

# truePosModel = 2, 4, or 5
# scoringModel = TRUE, or 6 [m11] or 7 [m1100] or [new!] 8 (cosine) or 9 (cosine, defining cosine(*,0) = 0) or 10 (pearsonWeighted)
# 		or 11 (m) or 12 (d) or 13 (adamic, with m passed in as a parameter) or 14 (jaccard) or 15 (pearson corr)
# (previous version: 9 meant "cosine2", which was a weird thing I tried once.
# useModel5Ts: if TRUE, plot axes are in model5 units
# note: at n=1, pathological things happen. Doesn't make sense to model the AUC as normal when each distribution has only 2 possible points.
createPrettyAUCPlotPiT = function(n, truePosModel = 5, scoringModel = TRUE, useModel5Ts = TRUE, moreTicks=0, quicker=F,
							conditionOnNonZero = F, mForAdamicNewman=2000) {
	# tmp tmp for quicker testing
	if (quicker) {
		#t = seq(from=.05, to=.95, by=.05)
		#pi = seq(from=.05, to=.95, by=.05)
		t = seq(from=.05, to=.95, by=.025)
		p_i = c(seq(from=.01, to=.19, by=.01), seq(from=.2, to=.95, by=.05))
	} else {
		t = seq(from=.01, to=.99, by=.01)
		p_i = seq(from=.01, to=.99, by=.01)
	}
	g = expand.grid(p_i=p_i, t=t, n=n)
	
	tInputToFns = g$t
	if (truePosModel != 5 && useModel5Ts) {
		tInputToFns = mapply(convertTFromModel5ConstPi, g$t, g$p_i)
	}

	if (scoringModel == TRUE) {
		if (truePosModel == 5) {
			if (conditionOnNonZero) {
				# new code, to see if this matches the IDF plot
				g$auc = mapply(AUC_pos5_constpi_notZero, p_i=g$p_i, trueT=g$t, MoreArgs = list(n=n))
			} else {
				g$auc = mapply(AUC_pos5_constpi, p_i=g$p_i, trueT=g$t, MoreArgs = list(n=n))
			}
		} else if (truePosModel == 4) {
			g$auc = mapply(AUC_pos4_constpi, trueT=tInputToFns, p_i=g$p_i, MoreArgs = list(n=n))
		} else if (truePosModel == 2) {
			g$auc = mapply(AUC_pos2_constpi, trueT=tInputToFns, p_i=g$p_i, MoreArgs = list(n=n))
		}
	} else if (scoringModel == 5) {
		if (truePosModel == 5) {
			g$auc = mapply(AUC_pos5_constpi, p_i=g$p_i, trueT=g$t, MoreArgs = list(n=n))
		} else if (truePosModel == 4) {
			g$auc = mapply(AUC_pos4_constpi, p_i=g$p_i, trueT=tInputToFns, MoreArgs = list(n=n))
		} else if (truePosModel == 2) {
			g$auc = mapply(AUC_pos2_constpi, p_i=g$p_i, trueT=tInputToFns, MoreArgs = list(n=n))
		}
	} else if (scoringModel == 6) {
		if (truePosModel == 5) {
			g$auc = mapply(AUC_pos5_m11_constpi, p_i=g$p_i, trueT=g$t, MoreArgs = list(n=n))
		} else if (truePosModel == 4) {
			g$auc = mapply(AUC_pos4_m11_constpi, p_i=g$p_i, trueT=tInputToFns, MoreArgs = list(n=n))
		} else if (truePosModel == 2) {
			g$auc = mapply(AUC_pos2_m11_constpi, p_i=g$p_i, trueT=tInputToFns, MoreArgs = list(n=n))
		}
	} else if (scoringModel == 7) {
		if (truePosModel == 5) {
			g$auc = mapply(AUC_pos5_m1100_constpi, p_i=g$p_i, trueT=g$t, MoreArgs = list(n=n))
		} else if (truePosModel == 4) {
			g$auc = mapply(AUC_pos4_m1100_constpi, p_i=g$p_i, trueT=tInputToFns, MoreArgs = list(n=n))
		} else if (truePosModel == 2) {
			g$auc = mapply(AUC_pos2_m1100_constpi, p_i=g$p_i, trueT=tInputToFns, MoreArgs = list(n=n))
		}
	} else if (scoringModel == 8) {
		if (truePosModel == 5) {
			g$auc = mapply(AUC_pos5_cosine_constpi, p_i=g$p_i, trueT=g$t, MoreArgs = list(n=n))
		}
	} else if (scoringModel == 9) {
		if (truePosModel == 5) {
			g$auc = mapply(AUC_pos5_cosine_constpi, p_i=g$p_i, trueT=g$t, MoreArgs = list(n=n, useCos0=T))
		}
	} else if (scoringModel == 10) {
		if (truePosModel == 5) {
			g$auc = mapply(AUCPearWeight_pos5_constpi, p_i=g$p_i, trueT=g$t, MoreArgs = list(n=n))
		}
	} else if (scoringModel == 11) {
		if (truePosModel == 5) {
			g$auc = mapply(AUC_pos5_m_constpi, p_i=g$p_i, trueT=g$t, MoreArgs = list(n=n))
		}
	} else if (scoringModel == 12) {
		if (truePosModel == 5) {
			g$auc = mapply(AUC_pos5_d_constpi, p_i=g$p_i, trueT=g$t, MoreArgs = list(n=n))
		}
	} else if (scoringModel == 13) {
		if (truePosModel == 5) {
			g$auc = mapply(AUC_pos5_adamic_constpi, p_i=g$p_i, trueT=g$t, MoreArgs = list(n=n, m=mForAdamicNewman))
		}
	} else if (scoringModel == 14) {
		if (truePosModel == 5) {
			g$auc = mapply(AUC_pos5_combinatorialScoreFn, p_i=g$p_i, trueT=g$t, MoreArgs = list(n=n, scoreFnName="jaccardScorePair"))
		}
	} else if (scoringModel == 15) {
		if (truePosModel == 5) {
			g$auc = mapply(AUC_pos5_combinatorialScoreFn, p_i=g$p_i, trueT=g$t, MoreArgs = list(n=n, scoreFnName="pearsonCorrPair"))
		}
	} else if (scoringModel == 16) {
		if (truePosModel == 5) {
			# sanity checking. This plot should match scoringModel9, cosine0.
			g$auc = mapply(AUC_pos5_combinatorialScoreFn, p_i=g$p_i, trueT=g$t, MoreArgs = list(n=n, scoreFnName="cosineScore2"))
		}
	}

	#title = paste("AUC predicted for constant p_i\nfor model ", truePosModel, " positives, n=", n, sep="")
	#if (scoringModel != T) {
	#	title = paste(title, ", scored with model ", scoringModel, sep="")
	#}
	#if (conditionOnNonZero) {
	#	title = paste(title, "\nconditioned on no vector being all 0's", sep="")
	#}

	# pretty ticks are easy for AUC. (Until we need extras.)
    at_vector = seq(from=min(.5, g$auc), to=1, by=.05)
	extraTicks = 1 - (.05 * .25**(1:moreTicks))	# magic incantation
	if (moreTicks > 0) {
		at_vector = c(at_vector[1:(length(at_vector)-1)], extraTicks, 1)
	}
	turnsInto1 = (signif(at_vector, 3) == 1)
    at_vector[!turnsInto1] = signif(at_vector[!turnsInto1], 3)    # if I use round, with small values they all turn into 1 and it gives a plotting error
    labels_print = as.character(at_vector)

	# hack for calling the variable s instead of t:
	g$s = g$t
	levelplot(auc ~ p_i * s, data=g, contour=T, col.regions=topo.colors(100, alpha=.3), xlab=expression(p[i]),
	#levelplot(auc ~ p_i * t, data=g, contour=T, col.regions=topo.colors(100, alpha=.3), xlab=expression(p[i]),
				panel = function(...) {
					panel.abline(v=0.5, lty = "dotted", col = "black")
					panel.levelplot(...)
				},
		# change font sizes:

        labels=labels_print, at=at_vector, label.style="align")
        #main=title)

}


