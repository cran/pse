plotecdf <- function (LHS, stack=FALSE, index.res =1:get.noutputs(LHS), col=index.res, ...) {
	if (stack) {
		dat <- vec(get.results(LHS)[,index.res])
		g <- rep(index.res, each=dim(LHS$res)[1])
		Ecdf(dat, group=g, col=col)
	} else Ecdf(get.results(LHS)[,index.res])
}

