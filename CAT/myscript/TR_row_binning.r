# Purpose   : perform binning of a signal
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: CM
# Version   : 1.0
# Licence   : GPL2.1

if(!exists('bin.set'))bin.set<-c(previous.name,'','','')
ans<-inpboxeeee(vlabel = c('*Matrix name','*First column','*Last column','*Bin width'),vinp=bin.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  bin.set<-ans
  M_<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  cl_start<-as.numeric(ans[[2]])
  cl_end<-as.numeric(ans[[3]])
  n<-cl_end-cl_start+1
  ampl<-as.numeric(ans[[4]])
  r<-n%%ampl
  if(!r==0){
    #print("the number of selected columns must be a multiple of the bin width")
    tk_messageBox(type=c("ok"),message="the number of selected columns must be a multiple of the bin width",caption="Input Error")
  }
  if(r==0){
    q_<-n/ampl
    X_<-as.data.frame(apply(M_[,cl_start:(cl_start+ampl-1)],1,'mean'))
    for (i in 2:q_){
      Y_<-as.data.frame(apply(M_[,(cl_start+ampl*(i-1)):(cl_start+ampl*i-1)],1,'mean'))
      X_<-cbind.data.frame(X_,Y_)
    }
    colnames(X_)<-paste0('X',1:q_)
    if(cl_start>1){
      M_1<-M_[,1:(cl_start-1)]
      X_<-cbind.data.frame(M_1,X_)}
    if(cl_end<ncol(M_)){
      M_2<-M_[,(cl_end+1):ncol(M_)]
      X_<-cbind.data.frame(X_,M_2)}
    assign(paste(ans[[1]],'.bin',sep=''),X_,envir=.GlobalEnv)
    print(paste('The value is saved in: ',ans[[1]],'.bin',sep=''))
  }
}
if(exists("ans"))rm(ans)
if(exists("cl_start"))rm(cl_start)
if(exists("cl_end"))rm(cl_end)
if(exists("ampl"))rm(ampl)
if(exists("M_"))rm(M_)
if(exists("X_"))rm(X_)
if(exists("Y_"))rm(Y_)
if(exists("q_"))rm(q_)
if(exists("r"))rm(r)
if(exists("i"))rm(i)
if(exists("n"))rm(n)
if(exists("M_1"))rm(M_1)
if(exists("M_2"))rm(M_2)

