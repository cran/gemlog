\name{Convert}
\alias{Convert}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert raw Gem data to segy}

\description{Convert takes a directory of raw Gem data files and converts them to PASSCAL segy files, including interpolating time with GPS strings and converting from counts to pressure units. }

\usage{
Convert(rawpath = ".", convertedpath = "converted", metadatapath = "metadata", 
metadatafile = NA, gpspath = "gps", gpsfile = NA, t1 = -Inf, t2 = Inf, nums = NaN, 
SN = NaN, bitweight = 0.256/2^15/(4.6e-05 * 3.4/7)/23.455, time_adjustment = 0, 
yr = 2016, blockdays = 1)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{rawpath}{
Directory containing raw data to be converted.
}
  \item{convertedpath}{
Directory (to be created, if necessary) where output segy files will be saved.
}
  \item{metadatapath}{
Directory (to be created, if necessary) where output metadata file will be saved.
}
  \item{metadatafile}{
Filename for output metadata. If set, overrides metadatapath. If unset, Convert creates the next logical filename in metadatapath. 
}
  \item{gpspath}{
Directory (to be created, if necessary) where output gps file will be saved.
}
  \item{gpsfile}{
Filename for output gps data. If set, overrides gpspath. If unset, Convert creates the next logical filename in gpspath. 
}
  \item{t1}{
Time at which conversion should start (class POSIXct).
}
  \item{t2}{
Time at which conversion should end (class POSIXct).
}
  \item{nums}{
File numbers to convert.
}
  \item{SN}{
Serial number of Gem data files to convert (to be safe, when data from multiple Gems could be mixed).
}
  \item{bitweight}{
Conversion factor between counts and Pascals (Pa per count).
}
  \item{time_adjustment}{
Offset to add to output times (usually unnecessary, sometimes +/- 1 s).
}
  \item{yr}{
Year of data (unnecessary).
}
  \item{blockdays}{
Amount of data (measured in days) to process at a time. Normally 1, but for computers with little memory, might be necessary to make less than 1.
}
}
\details{
This is the usual function to use when converting data to segy files. To read data directly into R, use ReadGem.
}
\value{
None; writes files only.
}
\author{
Jake Anderson
}
\note{
A good directory structure might be something like

projectname

----raw

--------010: Directory containing data from Gem SN 010 (e.g.)
  
--------011: Directory containing data from Gem SN 011 (e.g.)
  
----converted

--------segy files

----gps

--------010gps_000.txt: GPS file for Gem 010

--------011gps_000.txt: GPS file for Gem 011

----metadata

--------010metadata_000.txt: Metadata file for Gem 010

--------011metadata_000.txt: Metadata file for Gem 011

----projectname_notes.txt


}


\seealso{
ReadGem
}
\examples{
\dontrun{
# define bitweight for 0.5 inch sensor with Rg = 2.2k
sensitivity = 22.014e-6 # 22.014 uV/Pa
Rg = 2.2 # gain-setting resistor value in kilo-ohms
gain = 1 + 49.4/2.2 # amplifier gain
A2D = 0.256/2^15 # volts per count in analog-digital converter
bitweight = A2D / (gain * sensitivity) # conversion from counts to Pa (Pa/count)

# convert files from two Gems (SNs 000 and 001)
Convert('raw/000', bitweight = bitweight)
Convert('raw/001', bitweight = bitweight)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")

