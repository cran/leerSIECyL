leerCoyuntura <-
function(variables=NULL,periodos=NULL,nivel="COMUNIDAD AUTONOMA",zonas=NULL,frecuencia="TRIMESTRE"){
  # para evitar la consulta grande
  if(is.null(variables) && is.null(periodos)) return(NULL)
  # transformar la frecuencia
  fr=toupper(frecuencia)
  if(fr %in% c("A\U00D1O","YEAR","ANNO")) frecuencia="ANNO"
  if(fr %in% c("QUARTER","TRIMESTRE")) frecuencia="TRIMESTRE"
  if(fr %in% c("MONTH","MES")) frecuencia="MES"
  if(!(frecuencia %in% c("ANNO","TRIMESTRE","MES"))) return(NULL)   

  baseurl1="http://www.jcyl.es/sie/sas/broker"
  listaPar=list(
    "_PROGRAM"="sashelp.webeis.oprpt.scl",
    "_SERVICE"="saswebl",
    MDDB="COYWEB.M_COYUNTURA_WEB",
    CLASS="mddbpgm.jcyl.custom_webeis2.class",
    METABASE="RPOSWEB",
    ST="1",
    FS="SUM",
    SPDSHT="X",
    ACB=0,
    DC=0,  # para totales poner 1
    A="VALOR_SERIE",
    S="SUM",
    AC=paste0("COD_",gsub(" ","_",toupper(nivel))),
    D=c("COD_SERIE","COD_UNIDAD_MEDIDA",toupper(frecuencia)),
    "_SAVEAS"="fichero.csv")
  if(!is.null(variables)) listaPar=c(listaPar,list("SL"=paste0("COD_SERIE:",variables)))
  if(!is.null(periodos)) listaPar=c(listaPar,list("SL"=paste0(toupper(frecuencia),":",periodos)))
  if(!is.null(zonas)){
    listaPar=c(listaPar,list("SL"=paste0(paste0("COD_",gsub(" ","_",toupper(nivel))),":",toupper(zonas))))
  }
  texto=postForm(baseurl1,.params=listaPar,style = "POST",.encoding=2)
  datos=strsplit(texto,"\n")[[1]]
  datos=datos[datos!=""]
  options(warn=-1)
  datos=gsub("\"","",datos,fixed=TRUE)
  n1=strsplit(datos[2],",")[[1]]
  posDatos1=grep("^SERIE",datos)
  if(length(posDatos1)==0) return(NULL)
  campos=strsplit(datos[posDatos1],",")[[1]]
  campos[-(1:3)]=n1[-(1:3)]
  a1=strsplit(datos[-(1:posDatos1)],",")
  a1a=lapply(a1,function(x){
      c(paste(x[1:(length(x)-length(campos)+1)],collapse=","),x[-(1:(length(x)-length(campos)+1))])
  })
  datos1=data.frame(do.call(rbind,a1a),stringsAsFactors=FALSE) 
  datos1[,-(1:3)]=data.frame(lapply(datos1[,-(1:3),drop=FALSE],as.numeric))
  colnames(datos1)=campos
  #datos1=read.csv(text=datos[-(1:(posDatos1-1))],header=TRUE,na.string=".")
  cv=apply(datos1,2,function(x) all(is.na(x)))
  datos1=datos1[,!cv]
  datos1[,1]=rellenar(datos1[,1])
  datos1[,2]=rellenar(datos1[,2])
  datos1[,3]=rellenar(datos1[,3])
  #names(datos1)[-(1:3)]=n1[-(1:3)]
  # ponerlo en 5 columnas. La 1 el nombre de la zona y la 5 el valor
  datos2=do.call(rbind,lapply(colnames(datos1)[-(1:3)],function(x) data.frame(ZONA=x,datos1[,1:3],Valor=datos1[,x],check.names=FALSE)))
  return(datos2)
}
