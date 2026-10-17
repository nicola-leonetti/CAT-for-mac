# Purpose   : column centering 
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
if(!exists('cc.set'))cc.set<-c(previous.name,'','','')
ans<-inpboxeee('*Matrix name','*First column','*Last column',vinp=cc.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  cc.set<-ans
  var.trans<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  M<-var.trans[,ans[[2]]:ans[[3]],drop=FALSE]
  M<-scale(M,center=TRUE,scale=FALSE)
  var.trans[,ans[[2]]:ans[[3]]]<-M
  assign(paste(ans[[1]],'.centc',sep=''),var.trans,envir=.GlobalEnv)
  rm(M,var.trans)
  print(paste('The value is saved in: ',ans[[1]],'.centc',sep=''))
}