\name{rellenar}
\alias{rellenar}
\title{
Fill empty elements of a vector.
}
\description{
 Fill empty elements of a vector with the last non-empty previous
}
\usage{
rellenar(x)
}
\arguments{
  \item{x}{
The vector to be filled
}
}
\value{
  The vector filled.
}

\examples{
##Example
a1=c(2,NA,NA,3,NA,4,5,NA,NA)
rellenar(a1)
}
\keyword{ misc }
