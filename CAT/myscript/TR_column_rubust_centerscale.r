# Purpose   : robust column autoscaling
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
if(!exists('crc.set'))crc.set<-c(previous.name,'','','')
ans<-inpboxeee('*Matrix name','*First column','*Last column',vinp=crc.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  crc.set<-ans
  var.trans<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  M<-var.trans[,ans[[2]]:ans[[3]],drop=FALSE]
  M<-scale(M,center=apply(M,2,median),scale=apply(M,2,mad))
  var.trans[,ans[[2]]:ans[[3]]]<-M
  assign(paste(ans[[1]],'.rautosc',sep=''),var.trans,envir=.GlobalEnv)
  rm(M,var.trans)
  print(paste('The value is saved in: ',ans[[1]],'.rautosc',sep=''))
}




