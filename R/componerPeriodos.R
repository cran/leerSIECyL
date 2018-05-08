componerPeriodos <-
function(annos,mes=NULL,frecuencia="TRIMESTRE"){
    lf=toupper(substr(frecuencia,1,1))
    if(lf %in% c("A","Y")) return(annos)
    if(lf %in% c("T","Q")){
       return(as.vector(outer(paste0(mes,"\U00BA TRIM. "),annos,paste0)))  
    } 
    if(lf=="M"){
       if(is.null(mes)) mes=1:12
       meses=formatC(mes,format="d",width=2,flag="0")
       return(as.vector(outer(meses,annos,paste,sep="-")))
    } 
}
