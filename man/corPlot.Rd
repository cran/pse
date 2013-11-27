\name{corPlot}
\alias{corPlot}
\title{Produces a series of scatterplots from data
}
\description{Produces a series of scatterplots from data
}
\usage{
corPlot(vars, res = NULL, index.data, index.res, log, add.lm, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vars}{
	  Either a data.frame consisting of the data to plot on the x axis, or an LHS object
	  containing the data to be plot
%%     ~~Describe \code{vars} here~~
}
  \item{res}{
	  A data.frame consisting of the model results to be plotted on the y axis, if
	  vars is passed as a data.frame.
	  If vars is an LHS object, this parameter is ignored.
%%     ~~Describe \code{res} here~~
}
  \item{index.data}{
	  The indices of the data columns to be plotted. Useful when plotting from LHS objects
%%     ~~Describe \code{index.data} here~~
}
  \item{index.res}{
	  The indices of the result columns to be plotted. Useful when plotting from LHS objects
%%     ~~Describe \code{index.res} here~~
}
  \item{log}{
	  Whether the plots should be done on a logarithmic scale. See the help on plot.default()
%%     ~~Describe \code{log} here~~
}
  \item{add.lm}{
	  Boolean. Whether to include a simple linear model on the plots. Defaults to TRUE.
%%     ~~Describe \code{add.lm} here~~
}
  \item{\dots}{
	  Additional parameters to pass to the lower level plotting functions
%%     ~~Describe \code{\dots} here~~
}
}
