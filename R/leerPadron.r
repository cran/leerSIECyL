leerPadron=function(anno=NULL,nivel="PROVINCIA",provincias=NULL,municipios=NULL,
                    sexos=NULL,edades=NULL, gruposEdad=NULL,nacionalidad=NULL,total=FALSE){
  # nivel MUNICIPIO, PROVINCIA o COMUNIDAD
  # Si MUNICIPIO y MUNICIPIO=NULL se sacan tos los de las provincias 
  # Si nivel="PROVINCIA" se ignora lo que se ponga en municipios
  # si edades no es NULL se ignora gruposEdad
  # Si edades="TODAS" se sacan todas
  # los grupos son DE  0 A 4, DE  5 A 9, ... , DE 85 Y MAS
  # con "TODOS" se sacan todos
  # sexos puede ser "TODOS" "VARON" "MUJER"
  # nacionalidad puedes TODAS o vector de paises, si NULL no sale
  baseurl1="http://www.jcyl.es/sie/sas/broker"
  listaPar=list(
    "_PROGRAM"="sashelp.webeis.oprpt.scl",
    "_SERVICE"="saswebl",
    MDDB="PADRON.M_PADRON",
    CLASS="mddbpgm.jcyl.custom_webeis2.class",
    METABASE="RPOSWEB",
    ST="1",
    FS="SUM",
    SPDSHT="X",
    ACB=0,
    A="NUM_PERSONAS",
    S="SUM",
    D="ANNO",
    "_SAVEAS"="fichero.csv")

  if(!is.null(anno)) listaPar$SL=paste0("ANNO:",anno)

  resumir=FALSE
  buscar=""
  if(substr("MUNICIPIO",1,nchar(nivel))==toupper(nivel)){
     listaPar$D=c("COD_MUNICIPIO","COD_PROVINCIA",listaPar$D)
     buscar="MUNICIPIO"
     if(!is.null(municipios)){
         listaPar$SL=c(listaPar$SL,paste0("COD_MUNICIPIO:",toupper(trimws(municipios))))
     }else{
         if(!is.null(provincias)){
           listaPar$SL=c(listaPar$SL,paste0("COD_PROVINCIA:",toupper(trimws(provincias))))
         }
     }
  }
  if(substr("PROVINCIA",1,nchar(nivel))==toupper(nivel)){
     buscar="PROVINCIA"
     listaPar$D=c("COD_PROVINCIA",listaPar$D)
     if(!is.null(provincias)){
          listaPar$SL=c(listaPar$SL,paste0("COD_PROVINCIA:",toupper(trimws(provincias))))
      }
  }
  if(substr("COMUNIDAD",1,nchar(nivel))==toupper(nivel)){
     listaPar$D=c("COD_PROVICNIA",listaPar$D)
     buscar="PROVINCIA"
     resumir=TRUE
     total=TRUE
  }

  listaPar$DC=ifelse(total,1,0)
  
  if(!is.null(edades)){
      # las edades con cuatro caracteres relleno con blancos a la izquierda
      listaPar$D=c(listaPar$D,"EDAD")
      if(!("TODAS" %in% toupper(edades))){
          edades=paste0("   ",edades)
          edades=substr(edades,nchar(edades)-3,nchar(edades))
          listaPar$SL=c(listaPar$SL,paste0("EDAD:",edades))
      }
  }else{
     if(!is.null(gruposEdad)){
        # DE 10 a 14  el primer número siempre dos caracteres, 
        # hasta 9 dos espacios entre DE y el número 
        listaPar$D=c(listaPar$D,"COD_GRUPO_EDAD")  
        if(!("TODOS" %in% toupper(gruposEdad))){
          listaPar$SL=c(listaPar$SL,paste0("COD_GRUPO_EDAD:",toupper(gruposEdad)))
        }  
     }
  }

  if(!is.null(sexos)){
     listaPar$D=c(listaPar$D,"COD_SEXO")
     if(!("TODOS" %in% toupper(sexos))){
          listaPar$SL=c(listaPar$SL,paste0("COD_SEXO:",toupper(sexos)))
     }  
  }

  if(!is.null(nacionalidad)){
     listaPar$D=c(listaPar$D,"COD_PAIS_NACIMIENTO")
     if(!("TODAS" %in% toupper(nacionalidad))){
          listaPar$SL=c(listaPar$SL,paste0("COD_COD_PAIS_NACIMIENTO:",toupper(nacionalidad)))
     }  
  }

  #l1<<-listaPar
  texto=postForm(baseurl1,.params=listaPar,style = "POST",.encoding=2)
  datos=strsplit(texto,"\n")[[1]]
  datos=datos[datos!=""]
  options(warn=-1)
  datos=gsub("\"","",datos,fixed=TRUE)
  #n1=strsplit(datos[1],",")[[1]]
  posDatos1=grep(paste0("^",buscar),datos)
  if(length(posDatos1)==0) return(NULL)
  campos=strsplit(datos[posDatos1],",")[[1]]
  campos[-(1:(length(campos)-1))]="POBLACION" 
  a1=strsplit(datos[-(1:posDatos1)],",")
  datos1=data.frame(do.call(rbind,a1),stringsAsFactors=FALSE) 
  colnames(datos1)=campos
  datos1[,ncol(datos1)]=as.numeric(datos1[,ncol(datos1)])
  for(i in 1:(ncol(datos1)-1)) datos1[,i]=rellenar(datos1[,i])
  datos1=datos1[,!is.na(colnames(datos1))] 
  if(resumir){
    datos1=datos1[datos1$PROVINCIA=="TOTAL",]
    datos1$PROVINCIA="CASTILLA Y LEON"
    colnames(datos1)[1]="COMUNIDAD"
  }  
  #reshape(datos1,direction="wide",timevar="FECHA",
  #        idvar=colnames(datos1)[!(colnames(datos1) %in% c("FECHA","POBLACION"))])
  return(datos1)
}
