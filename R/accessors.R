get.results <-
function(LHS, get.mean=TRUE) { 
	# IF the object is incomplete (see tell method)
	if (is.null(dim(LHS$res))) {return (NA)}

	if(get.mean) {
		return (apply(LHS$res, c(1,2), mean)) 
	} else {
		return (LHS$res) 
	}
}

get.data <-
function(LHS) {
	return (LHS$data) 
}

get.N <- function(LHS) {return (dim(get.data(LHS))[1]) }
get.ninputs <- function(LHS) {return (dim(get.data(LHS))[2]) }
get.noutputs <- function(LHS) {return (dim(get.results(LHS))[2]) }
get.repetitions <- function(LHS) {return (dim(get.results(LHS, get.mean=FALSE))[3]) }
