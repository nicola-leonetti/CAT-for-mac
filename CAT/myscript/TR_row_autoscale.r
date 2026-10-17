# Purpose   : Standard Normal Variate (i.e., row autoscaling)
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.2
# Licence   : GPL2.1
#
if(!exists('ras.set'))ras.set<-c(previous.name,'','','')
ans<-inpboxeee('*Matrix name','*First column','*Last column',vinp=ras.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  ras.set<-ans
  var.trans<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  M<-var.trans[,ans[[2]]:ans[[3]],drop=FALSE]
  M<-t(M)
  M<-scale(M,center=TRUE,scale=TRUE)
  M<-t(M)
  var.trans[,ans[[2]]:ans[[3]]]<-M
  assign(paste(ans[[1]],'.snv',sep=''),var.trans,envir=.GlobalEnv)
  rm(M,var.trans)
  print(paste('The value is saved in: ',ans[[1]],'.snv',sep=''))
}
