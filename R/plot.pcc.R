# Based on code by Gilles Pujol 2006, modified by Andre Chalom 2013
plot.pcc <-
function(x, ylim = c(-1,1), main=(if ("PCC" %in% names(x)) "PCC" else "PRCC"), ...) {  
		if ("PCC" %in% names(x)) {
				nodeplot(x$PCC, ylim = ylim, main=main, ...)
		}else if ("PRCC" %in% names(x)) {
				nodeplot(x$PRCC, ylim = ylim, main=main, ...)
		}  
}
