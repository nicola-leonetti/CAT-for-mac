# Purpose   : second derivative (by row)
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.2
# Licence   : GPL2.1
#
if(!exists('rdf2.set'))rdf2.set<-c(previous.name,'','','')
ans<-inpboxeee('*Matrix name','*First column','*Last column',vinp=rdf2.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  rdf2.set<-ans
  var.trans<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  M<-var.trans[,ans[[2]]:ans[[3]],drop=FALSE]
  M<-t(M)
  M<-diff(M,lag=1,differences=2)
  M<-t(M)
  var.trans<-var.trans[,-as.numeric(ans[[2]])]
  var.trans<-var.trans[,-as.numeric(ans[[2]])]
  var.trans[,ans[[2]]:(as.numeric(ans[[3]])-2)]<-M
  assign(paste(ans[[1]],'.der2r',sep=''),var.trans,envir=.GlobalEnv)
  rm(M,var.trans)
  print(paste('The value is saved in: ',ans[[1]],'.der2r',sep=''))
}