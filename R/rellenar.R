rellenar <-
function(x){
  pos1=c(which(!is.na(x) & x!=""),length(x)+1)
  r1=pos1[-1]-pos1[-length(pos1)]
  x2=x[!is.na(x) & x!=""]
  x1=unlist(mapply(rep,x2,r1))
  dim(x1)=NULL
  names(x1)=NULL
  return(x1)
}
