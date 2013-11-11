corPlot <-
	function (vars, res=NULL, index.data=NULL, index.res=NULL,log="", add.lm=TRUE, ...) {
		if (class(vars)=="LHS") {
			if (is.null(index.data)) index.data <- 1:get.ninputs(vars)
			if (is.null(index.res)) index.res <- 1:get.noutputs(vars)
			data=get.data(vars)
			results=get.results(vars)
			corPlot(data[,index.data], results[,index.res], log=log, add.lm=add.lm, ...)
		}
		else {
			res <- as.data.frame(res)
			if (is.null(dim(vars))) {nplots <-1} else {nplots <- dim(vars)[2]}
			if (is.null(dim(res))) {nplots <-nplots*1} else {nplots <- nplots* dim(res)[2]}

			nl <- floor(sqrt(nplots))
			nc <- ceiling(nplots/nl)
			opar <- par(mfrow=c(nl,nc), pch='.', ...)
			on.exit(par(opar))

			index.var <- 1
			index.res <- 1
			for (i in 1:nl) for (j in 1:nc) {
				oneCorPlot(res[,index.res],vars[, index.var], c(names(vars)[index.var],names(res)[index.res]), log, add.lm, ...)
				index.res <- index.res +1
				if (index.res > dim(res)[2]) {index.res <- 1; index.var <- index.var + 1}
				if (index.var > dim(vars)[2]) break;
			}
		}
	}

oneCorPlot <-
function(res, var, name, log, add.lm, ...) {
		par (mar=c(4,4,2,0.5))
		plot(var,res, xlab=name[1], log=log, ylab= name[2], ...)
		if (add.lm) {
			l <- lm(res~var)
			if (!is.na(coefficients(l)[2])) 
					abline((lm(res~var)))
		}
}
