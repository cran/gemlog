\name{gemlog-package}
\alias{gemlog-package}
\alias{gemlog}
\docType{package}
\title{
\packageTitle{gemlog}
}
\description{
\packageDescription{gemlog}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{gemlog}
\packageIndices{gemlog}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
\packageAuthor{gemlog}

Maintainer: \packageMaintainer{gemlog}
}
\references{
~~ Literature or other references for background information ~~
}

\keyword{ package }
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

\dontrun{
ReadGem(0:1, 'raw/000') # read files raw/000/FILE0000.TXT and raw/000/FILE0001.TXT
}

}
