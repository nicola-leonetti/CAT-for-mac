# Purpose   : second derivative (by column)
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
if(!exists('cd2.set'))cd2.set<-c(previous.name,'','','')
ans<-inpboxeee('*Matrix name','*First column','*Last column',vinp=cd2.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  cd2.set<-ans
  var.trans<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  M<-var.trans[,ans[[2]]:ans[[3]],drop=FALSE]
  M<-as.matrix(M)
  M<-diff(M,lag=1,differences=2)
  var.trans<-var.trans[-c(1,2),]
  var.trans[,ans[[2]]:ans[[3]]]<-M
  assign(paste(ans[[1]],'.der2c',sep=''),var.trans,envir=.GlobalEnv)
  rm(M,var.trans)
  print(paste('The value is saved in: ',ans[[1]],'.der2c',sep=''))
}