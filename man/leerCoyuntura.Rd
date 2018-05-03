\name{leerCoyuntura}
\alias{leerCoyuntura}
\title{
Read data from the Coyuntura module.
}
\description{
Read data from the Coyuntura module, indicating the variables, periods, zone and frequency. 
}
\usage{
leerCoyuntura(variables = NULL, periodos = NULL, 
              nivel="COMUNIDAD AUTONOMA",zonas=NULL, frecuencia = "TRIMESTRE")
}
\arguments{
  \item{variables}{
    The names of the variables for read.
  }
  \item{periodos}{
    The periods for the data. 
    If the frequency is for years they are the numbers of the years, if it is quarterly the format is Nº TRIM. YYYY, N is the quarter and YYYY the year.
    If the frequency is monthly the format is MM-YYYY with MM being the month number, always with two digits: 01, 02, ..., 12.
  }
  \item{nivel}{
    The level of disaggregation : PROVINCIA or COMUNIDAD AUTONOMA
  }
  \item{zonas}{
    A vector with the provinces if nivel is "PROVINCIA" or "CASTILLA y LEON" y/o "ESPAÑA" if nivel is "COMUNIDAD AUTONOMA"   
  }
  \item{frecuencia}{
    The frequency of the data: YEAR (AÑO), QUARTER (TRIMESTRE) or MONTH (MES)
  }
}
\details{
  If variables, periodos or zonas are NULL the data obtained are all available without considering the relevant condition.
}
\value{
  A data.frame with the data.
If there is an error return NULLL.
The data.frame with 5 columns.
The columns are:
ZONA: the name of province or comunidad.
SERIE: The name of the variable.
UNIDAD DE MEDIDA: The units of variable.
ANNO or TRIMESTRE or MES : The period of the data.
Valor: The data.
}

\examples{
  ##-Examples
  \donttest{
    datos=leerCoyuntura(nivel="PROVINCIA",periodos="1º TRIM. 2017")
  }
}

\keyword{ datagen }