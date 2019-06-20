\name{GemSpecs}
\alias{GemSpecs}
\docType{data}
\title{Gem specs table}
\description{
  Specs table for various Gem versions.
}
\usage{data(GemSpecs)}
\format{
  Data frame with following elements:
  \describe{
    \item{ version }{ Version number corresponding to following specs
    }\item{ bitweight_Pa }{ Pressure resolution for default (high) gain (Pa/count)
    }\item{ bitweight_V }{ Voltage resolution for default (high) gain (V/count)
    }\item{ min_SN }{ Lowest serial number made for this version
    }\item{ max_SN }{ Highest serial number made for this version
    }
  }
}

\examples{
data(GemSpecs)

## determine pressure bitweight for logger 014
i = which(14 >= GemSpecs$min_SN & 14 <= GemSpecs$max_SN)
GemSpecs[i,]$bitweight_Pa

}
\keyword{datasets}
