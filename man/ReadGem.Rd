\name{ReadGem}
\alias{ReadGem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Read raw Gem data
}
\description{
	Reads raw Gem data into R. To write segy files, use Convert.
}
\usage{
ReadGem(nums = 0:9999, path = "/media/jake/SD/", alloutput = FALSE, 
verbose = TRUE, requireGPS = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nums}{
  File numbers to convert.
}
  \item{path}{
  Directory in which raw data files are contained.
}
  \item{alloutput}{
  Include raw data in the output, in addition to the processed data.
}
  \item{verbose}{
  Provide verbose output.
}
  \item{requireGPS}{
  Require GPS strings to perform the conversion.
}
}
\value{
\item{t}{sample times (POSIXct)}
\item{p}{samples (counts)}
\itemize{
	\item{gps$yr}{year of gps samples}
	\item{gps$date}{gps sample time, as fractional day of year}
	\item{gps$lat}{latitude}
	\item{gps$lon}{longitude}
}
\itemize{
	\item{metadata$millis}{millis count of metadata sample}
	\item{metadata$batt}{battery voltage}
	\item{metadata$temp}{temperature in (deg C)}
	\item{metadata$maxWriteTime}{maximum time required to write a sample}
	\item{metadata$minFifoFree}{minimum number of free sampes in FIFO buffer}
	\item{metadata$maxFifoUsed}{maximum number of used samples in FIFO buffer}
	\item{metadata$maxOverruns}{maximum number of sample overruns}
	\item{metadata$gpsOnFlag}{1 if gps is turned on, 0 otherwise}
	\item{metadata$unusedStack1}{free memory in stack 1}
	\item{metadata$unusedStackIdle}{free memory in idle stack}
}
\itemize{
	\item{header$file}{vector of raw file names}
	\item{header$SN}{vector of Gem serial numbers}
	\item{header$lat}{mean latitude}
	\item{header$lon}{mean longitude}
	\item{header$t1}{start time}
	\item{header$t2}{end time}
	\item{header$alloutput}{if alloutput == TRUE, list including raw data}
}
}
\author{
Jake Anderson
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Convert
}
\examples{
\dontrun{
ReadGem(0:1, 'raw/000') # read files raw/000/FILE0000.TXT and raw/000/FILE0001.TXT
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
