plotprcc <- function (LHS, index.res = 1:dim(LHS$res)[2], col='orange', ylab = NULL, ...) {
	nres <- length(index.res)
	nl <- floor(sqrt(nres))
	nc <- ceiling(nres/nl)
	opar <- par(mfrow=c(nl,nc))
	on.exit(par(opar))
	if(length(LHS$res.names) > 0 && is.null(ylab)) ylab = LHS$res.names[index.res]
	this.ylab= ylab
	for(i in 1:nres)
	{
		if (length(ylab) > 1) this.ylab = ylab[i]
		plot(LHS$prcc[[index.res[i]]], bg=col, ylab=this.ylab, ...)
		abline(h=0, lty=2)
	}
}

