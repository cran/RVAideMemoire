scat.mix.numeric <-
function(obj.dudi,xax=1,yax=2,...){
  indexation<-obj.dudi$index=="q"
  numero<-match(seq(1,length(obj.dudi$index))[indexation],obj.dudi$assign)
  noms<-row.names(obj.dudi$co[numero,c(xax,yax)])
  s.corcircle(obj.dudi$co[numero,c(xax,yax)],label=noms,...)
}

