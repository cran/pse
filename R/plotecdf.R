plotecdf <- function (LHS, stack=FALSE, index.res =1:get.noutputs(LHS), col=index.res, xlab = NULL, ...) {
	if (is.null (xlab)) xlab = LHS$res.names
	if (stack) {
		if (length(xlab) > 1) xlab = "LHS results"
		dat <- vec(get.results(LHS)[,index.res])
		g <- rep(index.res, each=dim(LHS$res)[1])
		Ecdf(dat, group=g, col=col, xlab=xlab, ...)
	} else Ecdf(get.results(LHS)[,index.res], xlab=xlab, ...)
}

