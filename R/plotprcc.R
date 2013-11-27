plotprcc <- function (LHS, index.res = 1:dim(LHS$res)[2], col='orange', ...) {
	nres <- length(index.res)
	nl <- floor(sqrt(nres))
	nc <- ceiling(nres/nl)
	opar <- par(mfrow=c(nl,nc), ...)
	on.exit(par(opar))
	name <- "PRCC"
	for(i in 1:nres)
	{
		if(length(LHS$res.names)) name = LHS$res.names[[index.res[i]]]
		plot(LHS$prcc[[index.res[i]]], bg=col, main=name)
		abline(h=0, lty=2)
	}
}

