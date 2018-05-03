\name{leerPadron}
\alias{leerPadron}

\title{
Read data from the Padron module.
}
\description{
Read data from the Padron module. 
}
\usage{
leerPadron(anno=NULL,nivel="PROVINCIA",provincias=NULL,municipios=NULL,
	       sexos=NULL,edades=NULL,gruposEdad=NULL,nacionalidad=NULL,total=FALSE)
}

\arguments{
  \item{anno}{
 A vector with the years for the data. If NULL read all available years.
  }
  \item{nivel}{
   The desaggregation nivel of the data: MUNICIPIO, PROVINCIA or COMUNIDAD. 
  }
  \item{provincias}{
A vector with the provinces for the data. If is NULL and nivel is PROVINCIA read all available provinces.
  }
  \item{municipios}{
A vector with the municipalities for the data. The name is of the form 'CCCCC Name', where CCCCC is the INE code and Name the name in uppercase of the municipality. The list of all municipalities can be obtained by setting nivel="MUNICIPIOS" and municipios=NULL.
  }
  \item{sexos}{
    If it is not null, the data is desegregated by sex. If it's TODOS they all come out, you can put VARON or MUJER. 
  }
  \item{edades}{
    If it is not null, the data is desegregated by age. If it's TODAS they all come out, you can put a vector of ages. 
  }
  \item{gruposEdad}{
    If it is not null, the data is desegregated by age group. If it's TODOS they all come out, you can put a vector of age groups. Id edades is not NULL is ignored.
  }
  \item{nacionalidad}{
    If it is not null, the data is desegregated by nationality. If it's TODAS they all come out, you can put a vector of nationalities.
  }
  \item{total}{
    If TRUE, the totals of each variable are output.
  }
}

\value{
A data.frame with the data.
If there is an error return NULLL.
}
\examples{
  ## Example
  \donttest{ 
    a1=leerPadron(anno=2015:2017,nivel="PROVINCIA",
                   municipios=NULL,gruposEdad="DE 10 A 14",total=TRUE)
  }  
}
\keyword{ datagen }
