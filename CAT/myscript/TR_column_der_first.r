# Purpose   : first derivative (by column)
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
if(!exists('cd1.set'))cd1.set<-c(previous.name,'','','')
ans<-inpboxeee('*Matrix name','*First column','*Last column',vinp=cd1.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  cd1.set<-ans
  var.trans<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  M<-var.trans[,ans[[2]]:ans[[3]],drop=FALSE]
  M<-as.matrix(M)
  M<-diff(M,lag=1,differences=1)
  var.trans<-var.trans[-1,]
  var.trans[,ans[[2]]:ans[[3]]]<-M
  assign(paste(ans[[1]],'.der1c',sep=''),var.trans,envir=.GlobalEnv)
  rm(M,var.trans)
  print(paste('The value is saved in: ',ans[[1]],'.der1c',sep=''))
}