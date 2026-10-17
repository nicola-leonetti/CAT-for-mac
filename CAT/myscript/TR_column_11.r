# Purpose   : column values in the range -1/1 (coding for DoE)
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
if(!exists('c11.set'))c11.set<-c(previous.name,'','','')
ans<-inpboxeee('*Matrix name','*First column','*Last column',vinp=c11.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  c11.set<-ans
  var.trans<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  M<-var.trans[,ans[[2]]:ans[[3]],drop=FALSE]
  if(sum(is.na(M))!=0){
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    tk_messageBox(type=c("ok"),message="NA found: remove them before evaluation",caption="Input Error")
    rm(ans,var.trans,M,opt)
    stop()
  }else{
    vmax<-apply(M,2,max)
    vmin<-apply(M,2,min)
    D<-vmax-vmin
    for(i in 1:ncol(M)){M[,i]<-2*(M[,i]-vmin[i])/D[i]-1}
    var.trans[,ans[[2]]:ans[[3]]]<-M
    assign(paste(ans[[1]],'.cod',sep=''),var.trans,envir=.GlobalEnv)
    rm(M,vmax,vmin,D,i,var.trans)
    print(paste('The value is saved in: ',ans[[1]],'.cod',sep=''))
  }
}