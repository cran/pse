target.sbma <- function(target, model, factors,  q = NULL, q.arg = NULL, res.names=NULL, COR=matrix(0, length(factors), length(factors)), eps=0.0005,init=length(factors)+2, inc=100, FUN=min) {
	#initial LHS
	N = init
	print("INFO: initial run...")
	oldL <- LHS(model, factors, N, q, q.arg, res.names, COR, eps, nboot=0)
	while (TRUE) {
		N = N + inc
		print(paste("INFO: LHS with N =", N));
		newL <- LHS(model, factors, N, q, q.arg, res.names, COR, eps, nboot=0)
		s <- FUN(sbma(newL, oldL))
		print(paste("sbma of ", round(s,3)," (target ",target,")", sep=""))
		if (s >= target) return (newL);
		oldL <- newL;
	}
}

