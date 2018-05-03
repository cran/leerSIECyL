\name{leerDB}
\alias{leerDB}

\title{
Read data from the Datos Básicos module.
}
\description{
Read data from the Datos Básicos module, indicating the years, the provinces and / or the municipalities and the variables that are to be obtained. 
}
\usage{
leerDB(anno = NULL, provincias = NULL, municipios = "TODOS", 
	   variables = "POBLACION DE DERECHO (TOTAL)")
}

\arguments{
  \item{anno}{
 A vector with the years for the data. If NULL read all available years 
  }
  \item{provincias}{
A vector with the provinces for the data. If is NULL read all available provinces. If not is NULL and municipios is NULL read data only for the provincies.
  }
  \item{municipios}{
A vector with the municipalities for the data. The name is of the form 'CCCCC Name', where CCCCC is the INE code and Name the name in uppercase of the municipality. The list of all municipalities can be obtained by setting "ALL" or "TODOS" in this parameter.
  }
  \item{variables}{
A vector with the names of the variables. The list of all variables can be obtained by setting NULL in this parameter. The default is 'POBLACIÓN DE DERECHO (TOTAL)'.
  }
}

\value{
A data.frame with the data.
If there is an error return NULLL.
The data.frame with 4 columns if municipios is NULL or 5 columns if it is not.
The columns are:
COD_MUNICIPIO: the name of municipality, if municipios not is NULL.
COD_PROVINCIA: The name of the province.
COD_ORDEN_VARIABLE: The name of the variable.
FECHA: The date of the data.
Valor: The data.
}
\examples{
  ## Example
  \donttest{ 
   datos=leerDB(2017,provincias=c("PALENCIA","VALLADOLID"))
   municipios=leerDB(2017)
   variables=leerDB(2016,provincias="VALLADOLID",municipios="NULL",variables=NULL)
  } 
}
\keyword{ datagen }
