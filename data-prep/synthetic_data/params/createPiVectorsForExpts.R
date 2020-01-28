pi_vectors = matrix(data = rep(seq(from=.1, to=.9, by=.1), each=100), nrow=79, ncol=100, byrow=T)
pi_vectors[10:19,] = runif(1000, min=0, max=1)
pi_vectors[20:29,] = runif(1000, min=0, max=.5)
pi_vectors[30:39,] = runif(1000, min=0, max=.1)
pi_vectors[40:49,] = rexp(1000, rate=5)
pi_vectors[which(pi_vectors > 1)] = rexp(sum(pi_vectors > 1), rate=5)	# (resampled invalid entries)
pi_vectors[50:59,] = rexp(1000, rate=10)
pi_vectors[60:69,] = rexp(1000, rate=50)
# note: I did manually check the last two blocks to make sure no entries were > 1.
pi_vectors[70:79,] = rbeta(1000, shape1=3, shape2=3)

save(pi_vectors, file="pi_vectors_forExpts.Rdata")
write.table(pi_vectors, file="test.csv", col.names=F, row.names=F, sep=",")
