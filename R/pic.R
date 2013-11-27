# Based on code by Gilles Pujol 2006
# Released originally as package sensitivity, licensed as GPL-2
### WORK IN PROGRESS: adaptar as funcoes para multiplas respostas
estim.pic <- function(data, i = 1:nrow(data)) {  
  d <- data[i, ]
  p <- ncol(d) - 1
  pic <- numeric(p)
  for (j in 1:p) {
    Xtildej.lab <- paste(colnames(d)[c(-1, -j-1)], collapse = "+")
    lm.Y <- lm(formula(paste(colnames(d)[1], "~", Xtildej.lab)), data = d)
    lm.Xj <- lm(formula(paste(colnames(d)[j+1], "~", Xtildej.lab)), data = d)
	y = d[1] - fitted(lm.Y)
	x = d[j+1] - fitted(lm.Xj)
	pic[j] <- coef(lm(y[,1] ~ x[,1]))[2]
  }
  return(pic)
}

pic <- function (LHS, nboot = 0, conf=0.95) {
	if (nboot == 0) {
		pic <- list()
		for(i in 1:dim(get.results(LHS))[2]) {
			data <- cbind(Y = get.results(LHS)[i], get.data(LHS))
			pic[[i]] <- estim.pic(data)
		}
		pic <- as.data.frame(pic)
		rownames(pic) <- colnames(get.data(LHS))
		colnames(pic) <- colnames(get.results(LHS))
	} else {
		data <- cbind(Y=get.results(LHS), get.data(LHS))
		## NAO funciona para mais de uma variavel resposta
		boot.pic <- boot(data, estim.pic, R = nboot)
		pic <- bootstats(boot.pic, conf, "basic")
		rownames(pic) <- colnames(get.data(LHS))
	}
	out <- list(X = get.data(LHS), y = get.results(LHS), nboot = nboot, conf = conf,
				call = match.call(), pic = pic)
	class(out) <- "pic"
	return(out)
}
