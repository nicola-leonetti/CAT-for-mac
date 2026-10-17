# Purpose   : extract some relevant features from a PLS object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.3
# Licence   : GPL2.1
#
if(exists("PLS",envir=.GlobalEnv)){
  if(PLS$typ=='PLS1'){
    ans<-inpboxc('* Extract Matrix:',c('RMSECV','y-Loadings','y-Scores','x-Loadings','Coefficients','MSEP',
                                       'Weights load.','Fitted Values','CV Values'))
    if(!is.null(ans)){
      if(ans[[1]]==1){rmsecv<-RMSEP(PLS$res)$val[1,,];print('Values saved and exported in "rmsecv"',quote=FALSE) }
      if(ans[[1]]==2){yLm<-Yloadings(PLS$res);print('Values saved and exported in "yLm"',quote=FALSE) }
      if(ans[[1]]==3){ySm<-Yscores(PLS$res);print('Values saved and exported in "ySm"',quote=FALSE) }
      if(ans[[1]]==4){xLm<-loadings(PLS$res);print('Values saved and exported in "xLm"',quote=FALSE) }
      if(ans[[1]]==5){
        if(is.null(PLS$res$scale))Cm<-c(coef(PLS$res,intercept=TRUE)[1],coef(PLS$res,intercept=FALSE))
        if(!is.null(PLS$res$scale))Cm<-c(coef(PLS$res,intercept=TRUE)[1],coef(PLS$res,intercept=FALSE)/PLS$res$scale)
        print('Values saved and exported in "Cm"',quote=FALSE) }
      if(ans[[1]]==6){msep<-MSEP(PLS$res)$val[1,,];print('Values saved and exported in "msep"',quote=FALSE) }
      if(ans[[1]]==7){wLm<-loading.weights(PLS$res);print('Values saved and exported in "wLm"',quote=FALSE) }
      if(ans[[1]]==8){Fv<-PLS$res$fitted.value;print('Values saved and exported in "Fv"',quote=FALSE) }  
      if(ans[[1]]==9){CVv<-PLS$res$validation$pred;print('Values saved and exported in "CVv"',quote=FALSE) }
    }
  }
  if(PLS$typ=='PCR'){
    ans<-inpboxc('* Extract Matrix:',c('RMSECV','y-Loadings','x-Loadings','Coefficients','MSEP',
                                       'Fitted Values','CV Values'))
    if(!is.null(ans)){
      if(ans[[1]]==1){rmsecv<-RMSEP(PLS$res)$val[1,,];print('Values saved and exported in "rmsecv"',quote=FALSE) }
      if(ans[[1]]==2){yLm<-Yloadings(PLS$res);print('Values saved and exported in "yLm"',quote=FALSE) }
      # if(ans[[1]]==3){ySm<-Yscores(PLS$res);print('Values saved and exported in "ySm"',quote=FALSE) }
      if(ans[[1]]==3){xLm<-loadings(PLS$res);print('Values saved and exported in "xLm"',quote=FALSE) }
      if(ans[[1]]==4){
        if(is.null(PLS$res$scale))Cm<-c(coef(PLS$res,intercept=TRUE)[1],coef(PLS$res,intercept=FALSE))
        if(!is.null(PLS$res$scale))Cm<-c(coef(PLS$res,intercept=TRUE)[1],coef(PLS$res,intercept=FALSE)/PLS$res$scale)
        print('Values saved and exported in "Cm"',quote=FALSE) }
      if(ans[[1]]==5){msep<-MSEP(PLS$res)$val[1,,];print('Values saved and exported in "msep"',quote=FALSE) }
      # if(ans[[1]]==7){wLm<-loading.weights(PLS$res);print('Values saved and exported in "wLm"',quote=FALSE) }
      if(ans[[1]]==6){Fv<-PLS$res$fitted.value;print('Values saved and exported in "Fv"',quote=FALSE) }  
      if(ans[[1]]==7){CVv<-PLS$res$validation$pred;print('Values saved and exported in "CVv"',quote=FALSE) }
    }
  }
}else{tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCR/PLS1/PLS2!',
caption="Input Error")}

if(PLS$typ=='PLS1'){
  if(ans[[1]]==1) {values="rmsecv";col=FALSE;row=TRUE}
  if(ans[[1]]==2) {values="yLm";col=TRUE;row=FALSE}
  if(ans[[1]]==3) {values="ySm";col=TRUE;row=TRUE}
  if(ans[[1]]==4) {values="xLm";col=TRUE;row=TRUE}
  if(ans[[1]]==5) {values="Cm";col=FALSE;row=FALSE}
  if(ans[[1]]==6) {values="msep";col=FALSE;row=TRUE}
  if(ans[[1]]==7) {values="wLm";col=TRUE;row=TRUE}
  if(ans[[1]]==8) {values="Fv";col=TRUE;row=TRUE}
  if(ans[[1]]==9) {values="CVv";col=TRUE;row=TRUE}
}
if(PLS$typ=='PCR'){
  if(ans[[1]]==1) {values="rmsecv";col=FALSE;row=TRUE}
  if(ans[[1]]==2) {values="yLm";col=TRUE;row=FALSE}
  # if(ans[[1]]==3) {values="ySm";col=TRUE;row=TRUE}
  if(ans[[1]]==3) {values="xLm";col=TRUE;row=TRUE}
  if(ans[[1]]==4) {values="Cm";col=FALSE;row=FALSE}
  if(ans[[1]]==5) {values="msep";col=FALSE;row=TRUE}
  # if(ans[[1]]==7) {values="wLm";col=TRUE;row=TRUE}
  if(ans[[1]]==6) {values="Fv";col=TRUE;row=TRUE}
  if(ans[[1]]==7) {values="CVv";col=TRUE;row=TRUE}
}

ans=list(values,".","NA",col,row)

variable<-NULL
if(!is.null(ans))variable<-makevar(ans[[1]])
if(!is.null(variable)){
	M<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
	if((as.logical(ans[[4]]))&(as.logical(ans[[5]]))){
		mt<-dimnames(M)[[2]]
		if(is.null(mt))mt<-as.character(1:ncol(M))
		mt<-matrix(c(' ',mt),1,ncol(M)+1)
		write.table(mt,paste(variable$name,'.txt',sep=''),sep="\t",quote=TRUE,row.names=FALSE,col.names=FALSE)
		write.table(M,paste(variable$name,'.txt',sep=''),sep="\t",quote=TRUE,append=TRUE,dec=as.character(ans[[2]]),na=as.character(ans[[3]]),row.names=TRUE,col.names=FALSE)
		rm(mt)
	}
	if((as.logical(ans[[4]]))&(!as.logical(ans[[5]]))){
		write.table(M,paste(variable$name,'.txt',sep=''),sep="\t",quote=TRUE,dec=as.character(ans[[2]]),na=as.character(ans[[3]]),row.names=FALSE,col.names=TRUE)
	}
	if((!as.logical(ans[[4]]))&(as.logical(ans[[5]]))){
		write.table(M,paste(variable$name,'.txt',sep=''),sep="\t",quote=TRUE,dec=as.character(ans[[2]]),na=as.character(ans[[3]]),row.names=TRUE,col.names=FALSE)
	}
	if((!as.logical(ans[[4]]))&(!as.logical(ans[[5]]))){
		write.table(M,paste(variable$name,'.txt',sep=''),sep="\t",quote=TRUE,dec=as.character(ans[[2]]),na=as.character(ans[[3]]),row.names=as.logical(ans[[5]]),col.names=as.logical(ans[[4]]))
	}
	assign('previous.name',variable$name,envir=.GlobalEnv,inherits=FALSE)
}else{
	tk_messageBox(type=c("ok"),message='The Variable does not exist!',caption="Input Error")
}

rm(values,ans,row,col)