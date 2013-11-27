\name{plotecdf}
\alias{plotecdf}
\alias{plotprcc}
\title{
	Uncertainty and sensitivity plots
}
\description{
	Plots the empirical cumulative density function
   or the partial rank correlation coefficient from an LHS object.
}
\usage{
plotecdf(LHS, stack = FALSE, index.res, col, ...)
plotprcc(LHS, index.res, col, ...)
}
\arguments{
  \item{LHS}{
	  The LHS object containing the simulation results to be plotted.
}
  \item{stack}{
	  If the results is a data.frame with several variables, stack=FALSE generates
	  a series of plots, and stack=TRUE generates a single plot with the ECDF from
	  all variables identified by different colors.
}
  \item{index.res}{
	  An optional vector indicating which columns from the results are to be plotted.
}
  \item{col}{
	  An optional vector indicating the colors to be used.
}
  \item{\dots}{
	  Additional parameters to be passed to the lower level plotting function.
}
}
