#########################################
############# External code #############
#########################################

# The functions in this file were copied from other
# packages, either to reduce the number of package
# dependencies, or because they are not exported in their
# package namespace. Original authors and licenses are
# given for all of them.

# Nodeplot by Gilles Pujol 2006
# Released in package sensitivity as GPL-2
nodeplot <-
function(x, xlim = NULL, ylim = NULL, labels = TRUE,
                     col = par("col"), pch = 22, bg = "white",
                     add = FALSE, at = NULL, ...) {
  n <- nrow(x)
  if (is.null(xlim)) {
    xlim <- c(1, n)
  }
  if (is.null(ylim)) {
    ylim <- c(min(x), max(x))
  }
  if (is.null(at)) {
    at <- 1 : n
  }
  if (add) {
    par(new = TRUE)
  }

  # axes
  
  plot(0, xlim = xlim, ylim = ylim, axes = FALSE,
       xlab = "", type = "n", ...)
  if (class(labels) == "logical") {
    if (labels) {
      axis(side = 1, at = at, labels = rownames(x))
    } else {
      axis(side = 1, at = at, labels = FALSE, tick = FALSE)
    }
  } else if (class(labels) == "character") {
    axis(side = 1, at = at, labels = labels)
  }
  axis(side = 2)
  box()

  # bias

  if ("bias" %in% colnames(x)) {
    xx <- x[["original"]] - x[["bias"]]
  } else {
    xx <- x[["original"]]
  }
  
  # confidence intervals

  if (("min. c.i." %in% colnames(x)) & "max. c.i." %in% colnames(x)) {
    for (i in 1 : n) {
      lines(c(at[i], at[i]), c(x[["min. c.i."]][i], x[["max. c.i."]][i]),
            col = col)
    }
  }

  # points

  points(at, xx, col = col, pch = pch, bg = bg)
}

# Vec operator by Tarn Duong
# Released in package ks, licensed as GPL-2/3
vec <- function(x, byrow=FALSE)
{
	  if (is.vector(x)) return (x)

  if (byrow) x <- t(x)
    d <- ncol(x)
    vecx <- vector()
	  for (j in 1:d)
		      vecx <- c(vecx, x[,j])

	  return(vecx)
}

#        Bootstrap statistics (overlay for the boot package)
#                         Gilles Pujol 2006
#        Released in package sensitivity, licensed as GPL-2


# bootstats(b, conf = 0.95, type = "norm")
# b : object of class 'boot'
# confidence : confidence level for bootstrap bias-corrected confidence
#   intervals
# type : type of confidence interval, "norm" or "basic"
#
# returns : a data.frame of bootstrap statistics

bootstats <- function(b, conf = 0.95, type = "norm") {
  p <- length(b$t0)
  lab <- c("original", "bias", "std. error", "min. c.i.", "max. c.i.")
  out <-  as.data.frame(matrix(nrow = p, ncol = length(lab),
                               dimnames = list(NULL, lab)))

  for (i in 1 : p) {
    
    # original estimation, bias, standard deviation
      
    out[i, "original"] <- b$t0[i]
    out[i, "bias"] <- mean(b$t[, i]) - b$t0[i]
    out[i, "std. error"] <- sd(b$t[, i])
      
    # confidence interval

    if (type == "norm") {
      ci <- boot.ci(b, index = i, type = "norm", conf = conf)
      if (!is.null(ci)) {
        out[i, "min. c.i."] <- ci$norm[2]
        out[i, "max. c.i."] <- ci$norm[3]
      }
    } else if (type == "basic") {
      ci <- boot.ci(b, index = i, type = "basic", conf = conf)
      if (!is.null(ci)) {
        out[i, "min. c.i."] <- ci$basic[4]
        out[i, "max. c.i."] <- ci$basic[5]
      }
    }
  }
  
  return(out)
}
