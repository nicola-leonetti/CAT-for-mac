# Purpose   : perform a permutation test
# Input     : object PLS
# Output    : plot with the distribution density and percentage of occurrencies "better" than the "real" value 
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: CM
# Version   : 1.3
# Licence   : GPL2.1

if(exists("PLS",envir=.GlobalEnv)){
ans<-inpboxe('Number of Permutations',100,'Input')
if(!is.null(ans)){
  Y_<-PLS$dataset[,1,drop=FALSE]
  X_<-PLS$dataset[,-1,drop=FALSE]
  D<-data.frame(NULL)
  pb<-winProgressBar("computation progress bar", "Some information in %",
                     min = 0, max = as.numeric(ans[[1]]), initial = 0, width = 300)
  for ( k in 1:as.numeric(ans[[1]])){
    a=as.numeric(Sys.time())
    set.seed(a)
    Y_=Y_[sample(nrow(Y_)),,drop=FALSE]
    X_=X_[sample(nrow(X_)),,drop=FALSE]
    Data<-cbind.data.frame(Y_,X_)
    Data <- data.frame(Data)
    res<-plsr(formula = PLS$model, ncomp = PLS$ncomp, data = Data,     
              scale = PLS$scale, validation = "CV", segment.type = "interleaved",segments = PLS$nseg)
    rmsep<-RMSEP(res,intercep=FALSE)
    D<-rbind.data.frame(D,rmsep$val[1,,PLS$ncomp])
    setWinProgressBar(pb, k,label=sprintf("%d%% done", round(k/as.numeric(ans[[1]])*100)))
  }
  close(pb)
  colnames(D)<-'RMSECV'
  dev.new(title="PLS permutation test")
  plot(main='Distribution density',xlab='RMSECV',density(D$RMSECV),xlim=c(min(PLS$rmsep,D$RMSECV),max(PLS$rmsep,D$RMSECV)))
  abline(v=PLS$rmsep, col="blue")
    print(paste0('Randomizations with RMSECV < than original data: ',sum(D$RMSECV<=PLS$rmsep)*100/as.numeric(ans[[1]]),'%'))
  rm(ans)
}
}else{
  tk_messageBox(type=c("ok"),message="Run Matrix Evaluation First in PLS1!",caption="Input Error")}


