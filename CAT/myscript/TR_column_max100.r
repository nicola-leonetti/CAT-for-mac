# Purpose   : column maximum equal to 100
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
if(!exists('cm100.set'))cm100.set<-c(previous.name,'','','')
ans<-inpboxeee('*Matrix name','*First column','*Last column',vinp=cm100.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  cm100.set<-ans
  var.trans<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  M<-var.trans[,ans[[2]]:ans[[3]],drop=FALSE]
  if(sum(is.na(M))!=0){
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    tk_messageBox(type=c("ok"),message="NA found: remove them before evaluation",caption="Input Error")
    rm(ans,var.trans,M,opt)
    stop()
  }else{
    sv<-apply(M,2,max)
    for(i in 1:ncol(M)){M[,i]<-M[,i]/sv[i]*100}
    var.trans[,ans[[2]]:ans[[3]]]<-M
    assign(paste(ans[[1]],'.max100c',sep=''),var.trans,envir=.GlobalEnv)
    rm(M,sv,i,var.trans)
    print(paste('The value is saved in: ',ans[[1]],'.max100c',sep=''))
  }
}