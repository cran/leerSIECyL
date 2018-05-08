leerDB <-
function(anno=as.numeric(substr(Sys.Date(),1,4))-1,provincias=NULL,municipios="TODOS",variables="POBLACION DE DERECHO (TOTAL)"){
  # chapuza para corregir errores al chequear
  variables=gsub("POBLACION","POBLACI%D3N",variables)
  baseurl1="http://www.jcyl.es/sie/sas/broker"
  listaPar=list(
    "_PROGRAM"="sashelp.webeis.oprpt.scl",
    "_SERVICE"="saswebl",
    MDDB="VARANU.MDDB_VARIABLES_ANUALES",
    CLASS="mddbpgm.jcyl.custom_webeis2.class",
    METABASE="RPOSWEB",
    ST="1",
    FS="SUM",
    SPDSHT="X",
    ACB=0,
    DC=0,  # para totales poner 1
    A="VALOR_VARIABLE",
    S="SUM",
    D=c("COD_PROVINCIA","COD_ORDEN_VARIABLE","FECHA"),
    "_SAVEAS"="fichero.csv")
  if(!is.null(variables)) listaPar=c(listaPar,list("SL"=paste0("COD_ORDEN_VARIABLE:",variables)))
  if(!is.null(anno)) listaPar$SL=c(listaPar$SL,paste0("FECHA:",anno))
  buscar="PROVINCIA"  
  if(!is.null(provincias)){
     if(!("TODAS" %in% toupper(provincias)) && !("ALL" %in% toupper(provincias))) listaPar$SL=c(listaPar$SL,paste0("COD_PROVINCIA:",toupper(provincias)))  
  }   
  if(!is.null(municipios)){
      buscar="MUNICIPIO"
      listaPar$D=c("COD_MUNICIPIO",listaPar$D)
      if(!("TODOS" %in% toupper(municipios)) && !("ALL" %in% toupper(municipios))) listaPar$SL=c(listaPar$SL,paste0("COD_MUNICIPIO:",toupper(municipios)))
  }  
  texto=postForm(baseurl1,.params=listaPar,style = "POST")
  datos=strsplit(texto,"\n")[[1]]
  datos=datos[datos!=""]
  options(warn=-1)
  datos=gsub("\"","",datos,fixed=TRUE)
  posDatos1=grep(paste0("^",buscar),datos)
  if(length(posDatos1)==0) return(NULL)
  
  campos=strsplit(datos[posDatos1],",")[[1]]
  
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
  if(!is.null(municipios)) datos1[,3]=rellenar(datos1[,3])
  names(datos1)[names(datos1)=="Sum"]="Valor"
  return(datos1)
}
