\name{componerPeriodos}
\alias{componerPeriodos}
\title{Function to create a vector of texts with the periods to consult}
\description{
   Function to create a vector of texts with the periods to consult.
}
\usage{
componerPeriodos(annos, mes = NULL, frecuencia = "TRIMESTRE")
}
\arguments{
  \item{annos}{
     A vector with the years to consult.
  }
  \item{mes}{
     Number to indicate the periods within the year to consult. 
  }
  \item{frecuencia}{
     Type of period. Year (A), Quarter (T) or Month (M)
  }
}
\details{
    The parameter mes is a number between 1 and 4 if the patameter frecuencia is Quarterly, from 1 to 12 if it is monthly and it is not used if it is annual,
    he frequency parameter is a text of which only the first letter is used: A or Y for annual, Q or T for quarterly or M for monthly.
}
\value{
    A vector with the periods in the format neccessary for the consult.
}
\examples{
##----Example
   componerPeriodos(2007:2009,2:4,'Q')
   componerPeriodos(2017,frecuencia='M')
   componerPeriodos(2017,frecuencia='A')
}
\keyword{ misc }