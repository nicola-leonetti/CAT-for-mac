# Purpose   : plot coefficient vectors with requested number of latent variables 
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: CM
# Version   : 1.1
# Licence   : GPL2.1
#
if(exists("PLS",envir=.GlobalEnv)){
# ans<-gnfib('*Number of latent variables (e.g.,5,6,7)','')
ans<-inpboxek('*Number of latent variables (e.g.,5,6,7)','Header on the x axis?',vinp=c('1,2','FALSE'),title='Input')
if(!is.null(ans)){
  at <- 1:length((colnames(PLS$dataset)[-PLS$nY]))
  tk <- TRUE
  # if(length((colnames(M_))>15))tk <- FALSE
  # if(length((colnames(M_)))>9000000000)at=c(seq(1,length(colnames(M_)),(length(colnames(M_))-1)/10),length(colnames(M_)))
  if(length(colnames(PLS$dataset)[-PLS$nY])>15){
    n<-length((colnames(PLS$dataset)[-PLS$nY]))
    m<-floor((n-1)/11)
    at=seq(1,n,m)
    rm(n,m)
  }
  assex <- 1:length(colnames(PLS$dataset)[-PLS$nY])
  x_lab <- 'Variable Number'
  if(ans[[2]]){
    assex <- colnames(PLS$dataset)[-PLS$nY]
    x_lab <- ''
  }
  if (is.null(PLS$res$scale))Cm<-PLS$res$coefficients[,1,]
  if (!is.null(PLS$res$scale))Cm<-PLS$res$coefficients[,1,]/PLS$res$scale
  vi<-as.numeric(unlist(str_split(ans[[1]],',')))
  #print(Cm)
  nCm<-length(Cm[,1])
  dev.new(title="calibration coefficients")
  plot(1:nCm,Cm[,1],xlab=x_lab,ylab='Regression Coefficients',type='n',xaxt='n',
       ylim=c(min(Cm[,vi]),max(Cm[,vi])));grid()
  axis(side=1, at=at,labels=assex[at],cex.axis=0.8,tick=tk)
  for(i in vi)lines(Cm[,i],col=which(vi==i))
  legend("bottomleft",legend=as.character(vi),col=1:length(vi),lty=1)
  abline(0,0,lty=2)
  rm(at,assex,tk,x_lab)
  rm(nCm,Cm,ans,i,vi)
  }
}else{tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCR/PLS1!',
                   caption="Input Error")}
  
  
  

