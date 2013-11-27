\name{pic}
\alias{pic}
\title{
	Partial Inclination Coefficient
}
\description{
	Estimates the partial inclination coefficient of a model response
	in relation with all model input variables.
}
\usage{
pic(LHS, nboot = 0, conf = 0.95)
}
\arguments{
  \item{LHS}{
	  The LHS object containing the input and output for the model.
}
  \item{nboot}{
	  Number of bootstrap replicates
}
  \item{conf}{
	  Confidence of the bootstrap interval
}
}
\details{
	Warning: in the current version, bootstraping only works for models with 
	a single output variable. This will be fixed in later versions of the
	package.
}
\author{
	Function by Andre Chalom, based on pcc function on sensitivity package
	by Gilles Pujol, Bertrand Iooss, Alexandre Janon.
}
