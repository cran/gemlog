\name{PlotMetadata}
\alias{PlotMetadata}
\alias{ScanMetadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Read and plot Gem metadata
}
\description{
	ScanMetadata reads a Gem metadata file produced by Convert.
	PlotMetadata plots battery, temperature, GPS metadata.
}
\usage{
ScanMetadata(fn, plot = TRUE)
PlotMetadata(M, xlim = range(M$t))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fn}{
  Filename to read
}
  \item{plot}{
  If true, plots metadata after reading the file
}
  \item{M}{
  Metadata, such as output of ScanMetadata
}
  \item{xlim}{
  Time limits to plot, in fractional days of year
}
}
\value{
  ScanMetadata: list including metadata from file:
	\item{millis}{millis count of metadata sample}
	\item{batt}{battery voltage}
	\item{temp}{temperature in (deg C)}
	\item{maxWriteTime}{maximum time required to write a sample}
	\item{minFifoFree}{minimum number of free sampes in FIFO buffer}
	\item{maxFifoUsed}{maximum number of used samples in FIFO buffer}
	\item{maxOverruns}{maximum number of sample overruns}
	\item{gpsOnFlag}{1 if gps is turned on, 0 otherwise}
	\item{unusedStack1}{free memory in stack 1}
	\item{unusedStackIdle}{free memory in idle stack}

  PlotMetadata: None.
}
\author{
Jake Anderson
}
\examples{
\dontrun{
M = ScanMetadata('metadata/001metadata_000.txt') # scan the first metadata file from Gem SN 001
}

data(gem_metadata)
PlotMetadata(gem_metadata)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO }% use one of  RShowDoc("KEYWORDS")
\keyword{plot }% __ONLY ONE__ keyword per line
