
# The best of pre-existing perComponentCalcs*, redone for prime time.
# NB: Make sure all log calls are log2(), for consistency.

source("/Users/lfriedl/Documents/dissertation/binary-ndim/code2/analytics3.R")

p_i = seq(0,1, length=1001)
outdir = "/Users/lfriedl/Documents/dissertation/text/documents/2020\ paper\ 1\ -\ KDD/figures"


plot_one_setting = function(p_i, t) {

    llr00 = log2(LR00_pos5(p_i=p_i, t=t))
    llr10 = log2(LR10_pos5(p_i=p_i, t=t))
    llr11 = log2(LR11_pos5(p_i=p_i, t=t))
    
	par(mar=c(3,3,3,1), mgp=c(2, .5, 0), cex=1.3)
    plot(p_i, llr00, type="l", 
		#ylim=range(c(-1, llr00[2:1000], llr10[2:1000], llr11[2:1000])), 
		ylim=c(-1, 8),
		xlab=expression(p[i]), ylab=expression(paste("score(", b[i], ")")),
         main=paste("MixedPairs LLR scores, s = ", t, sep=""), 
		col="brown")
    lines(p_i, llr10, type="l", col="blue")
    lines(p_i, llr11, type="l", col="green")
    legend("top", legend=c("1|1", "0|0", "1|0 or 0|1"),
           lty=1, col=c("green", "brown", "blue"))
    abline(h=0, lty=3)
}

plot_several_settings = function(p_i) {
	# start with existing t = .2, then add .5, .8
	t = .2
    llr00 = log2(LR00_pos5(p_i=p_i, t=t))
    llr10 = log2(LR10_pos5(p_i=p_i, t=t))
    llr11 = log2(LR11_pos5(p_i=p_i, t=t))

	par(mar=c(3,3,3,1), mgp=c(2, .5, 0), cex=1.3)
    plot(p_i, llr00, type="l", lty=2,
		#ylim=range(c(-1, llr00[2:1000], llr10[2:1000], llr11[2:1000])), 
		ylim=c(-5, 10),
		xlab=expression(p[i]), ylab=expression(paste("score(", b[i], ")")),
         main="MixedPairs LLR scores, s = (0.2, 0.5, 0.8)",
		col="brown")

    lines(p_i, llr10, type="l", col="blue", lty=2)
    lines(p_i, llr11, type="l", col="green", lty=2)
    abline(h=0, lty=3)

	for (t in c(.5, .8)) {

		llr00 = log2(LR00_pos5(p_i=p_i, t=t))
		llr10 = log2(LR10_pos5(p_i=p_i, t=t))
		llr11 = log2(LR11_pos5(p_i=p_i, t=t))
		
		lines(p_i, llr00, type="l", col="brown", lty=2)
		lines(p_i, llr10, type="l", col="blue", lty=2)
		lines(p_i, llr11, type="l", col="green", lty=2)
	}
	# text() labels...
	text("s=0.2", x=.04, y=1.5, cex=.9)
	text("s=0.5", x=.9, y=-1.4, cex=.9)
	text("s=0.8", x=.9, y=-2.7, cex=.9)

	llrM1100_00 = log2(LR00_m1100(p_i))
	#llrM1100_10 = rep(log2(1), length(p_i))	# (won't be plotted)
	llrM1100_11 = log2(LR11_m1100(p_i))

    lines(p_i, llrM1100_00, type="l", col="brown", lty=1)
    lines(p_i, llrM1100_11, type="l", col="green", lty=1)


	legend("top", legend=c("1|1", "0|0", "1|0 or 0|1"),
	   lty=1, col=c("green", "brown", "blue"))

}

# basic plot, t = 0.2
if (F) {
	pdf(paste(outdir, "/logLR_t", t, ".pdf", sep=""), height=6, width=6)
	t = .2
	plot_one_setting(p_i, t) 
	dev.off()
}

# plot with many t's, converging to m1100
if (F) {
	pdf(paste(outdir, "/logLR_manyT.pdf", sep=""), height=6, width=6)
	plot_several_settings(p_i)
	dev.off()
}

