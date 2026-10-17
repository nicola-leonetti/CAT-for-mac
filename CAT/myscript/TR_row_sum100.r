# Purpose   : row sum equal to 100
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.2
# Licence   : GPL2.1
#


if(!exists('rs100.set'))rs100.set<-c(previous.name,'','','')
ans<-inpboxeee('*Matrix name','*First column','*Last column',vinp=rs100.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  rs100.set<-ans
  var.trans<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  cl_bl<-ncol(var.trans)
  M<-var.trans[,ans[[2]]:ans[[3]],drop=FALSE]
  if(sum(is.na(M))!=0){
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    #print("NA found: remove them before evaluation")
    tk_messageBox(type=c("ok"),message="NA found: remove them before evaluation",caption="Input Error")
    rm(ans,var.trans,cl_bl,M,opt)
    stop()
  }
  sv<-apply(M,1,sum)
  for(i in 1:nrow(M)){M[i,]<-M[i,]/sv[i]*100}
  var.trans[,ans[[2]]:ans[[3]]]<-M
  assign(paste(ans[[1]],'.sum100r',sep=''),var.trans,envir=.GlobalEnv)
  rm(M,sv,i,var.trans,cl_bl)
  print(paste('The value is saved in: ',ans[[1]],'.sum100r',sep=''))
  }