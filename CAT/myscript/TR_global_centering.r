# Purpose   : global centering
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
if(!exists('gc.set'))gc.set<-c(previous.name,'','','')
ans<-inpboxeee('*Matrix name','*First column','*Last column',vinp=gc.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  gc.set<-ans
  var.trans<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  M<-as.matrix(var.trans[,ans[[2]]:ans[[3]],drop=FALSE])
  if(sum(is.na(M))!=0){
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    tk_messageBox(type=c("ok"),message="NA found: remove them before evaluation",caption="Input Error")
    rm(ans,var.trans,M,opt)
    stop()
  }else{
    M<-M-mean(M)
    var.trans[,ans[[2]]:ans[[3]]]<-M
    assign(paste(ans[[1]],'.gcent',sep=''),var.trans,envir=.GlobalEnv)
    rm(M,var.trans)
    print(paste('The value is saved in: ',ans[[1]],'.gcent',sep=''))
  }
}