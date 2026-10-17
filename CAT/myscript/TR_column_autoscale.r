# Purpose   : column autoscaling
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.0
# Licence   : GPL2.1
#
if(!exists('cas.set'))cas.set<-c(previous.name,'','','')
ans<-inpboxeee('*Matrix name','*First column','*Last column',vinp=cas.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  cas.set<-ans
  var.trans<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  M<-var.trans[,ans[[2]]:ans[[3]],drop=FALSE]
  M<-scale(M,center=TRUE,scale=TRUE)
  var.trans[,ans[[2]]:ans[[3]]]<-M
  assign(paste(ans[[1]],'.autosc',sep=''),var.trans,envir=.GlobalEnv)
  rm(M,var.trans)
  print(paste('The value is saved in: ',ans[[1]],'.autosc',sep=''))
}