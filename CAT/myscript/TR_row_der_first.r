# Purpose   : first derivative (by row)
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.2
# Licence   : GPL2.1
#
if(!exists('rdf.set'))rdf.set<-c(previous.name,'','','')
ans<-inpboxeee('*Matrix name','*First column','*Last column',vinp=rdf.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  rdf.set<-ans
  var.trans<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  M<-var.trans[,ans[[2]]:ans[[3]],drop=FALSE]
  M<-t(M)
  M<-diff(M,lag=1,differences=1)
  M<-t(M)
  var.trans<-var.trans[,-as.numeric(ans[[2]])]
  var.trans[,ans[[2]]:(as.numeric(ans[[3]])-1)]<-M
  assign(paste(ans[[1]],'.der1r',sep=''),var.trans,envir=.GlobalEnv)
  rm(M,var.trans)
  print(paste('The value is saved in: ',ans[[1]],'.der1r',sep=''))
}