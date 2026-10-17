# Purpose   : draw a Mean Squared Error of Prediction in Cross-Validation vs.the
#             component number using the PLS1 method 
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 2.5
# Licence   : GPL2.1
#
suppressWarnings(suppressPackageStartupMessages(library("Rcpp")))
library(pls)
library(gplots)
require(pcaMethods)
if(!exists('pls.set'))pls.set<-c(previous.name,'all','','','10','5','TRUE')
ans<-inpboxe6k1(c('*Matrix Name','*Rows (e.g., "1:10,15" or "15:end")',
'*X-Variables (e.g., "1:3,7" or "3:end")','*Y-Variable (e.g., 9)',
'*Number of Components','*Number of Segments for CV'),'Scaled (centering by default)',pls.set)
if(!is.null(ans)){
 PLS<-list()
 previous.name<-ans[[1]]
 M_<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)

.a <- strsplit(unlist(strsplit(ans[[2]], ',')),':')
if((ans[[2]]!='all')& .a[[length(.a)]][length(.a[[length(.a)]])]=='end'){
  .a[[length(.a)]][2]=nrow(M_)
  for(i in 1:length(.a)).a[[i]]<-paste(.a[[i]],collapse=':')
  ans[[2]]<-paste(unlist(.a),collapse=',')
}
.a <- strsplit(unlist(strsplit(ans[[3]], ',')),':')
if((ans[[3]]!='all')& .a[[length(.a)]][length(.a[[length(.a)]])]=='end'){
  .a[[length(.a)]][2]=ncol(M_)
  for(i in 1:length(.a)).a[[i]]<-paste(.a[[i]],collapse=':')
  ans[[3]]<-paste(unlist(.a),collapse=',')
}
rm(.a)

 if(ans[[4]]!='None'){
    if(is.na(suppressWarnings(as.numeric(ans[[4]])))){
	    Y_<-eval(parse(text=ans[[4]]),envir=.GlobalEnv)
    }else{
	    Y_<-M_[,as.numeric(ans[[4]])] 
		if(ans[[2]]!='all')Y_<-Y_[givedim(ans[[2]])]
	}
 }else{
	Y_<-rep(0,nrow(M_))
	if(ans[[2]]!='all')Y_<-Y_[givedim(ans[[2]])]
 }

 if((ans[[2]]!='all')&(ans[[3]]!='all'))M_<-M_[givedim(ans[[2]]),givedim(ans[[3]])]
 if((ans[[2]]!='all')&(ans[[3]]=='all'))M_<-M_[givedim(ans[[2]]),]
 if((ans[[2]]=='all')&(ans[[3]]!='all'))M_<-M_[,givedim(ans[[3]])]
 if((typeof(M_)=='double')|(typeof(M_)=='list')){
 # M_<-data.frame(cbind(Y_,data.frame(M_)))
 if(sum(is.na(Y_))>0){
  M_<-M_[-which(is.na(Y_)),]
  Y_<-Y_[-which(is.na(Y_))]
 }  
 M_<-cbind.data.frame(Y_,M_)
 if(ans[[4]]!='None')colnames(M_)[1]<-colnames(eval(parse(text=ans[[1]]),envir=.GlobalEnv))[as.numeric(ans[[4]])]
 naM_0<-names(M_)
 nNA<-sum(is.na(M_))
 nY<-1
 M_ <- data.frame(M_)
 naM<-names(M_)
 if(nNA>0){
 mess<-paste(as.character(nNA),'NA present.We try to rebuild them!')
 tk_messageBox(type=c("ok"),message=mess,caption="Input Error")
 md<-prep(M_,scale="uv",center=TRUE,simple=FALSE,rev=FALSE)
 res<-pca(md$data,method="nipals",nPcs=min(ncol(M_),10),scale="uv",center=TRUE)
 M_<-prep(res@completeObs,scale=md$scale,center=md$center,reverse=TRUE)
 M_<-as.data.frame(M_)
 rm(md,res)}
 ncompo<-min(as.numeric(ans[[5]]),ncol(M_)-1)
 model<-paste(naM[nY],'~',(paste(naM[-nY],collapse='+')),sep='')
 res<-plsr(as.formula(model),ncomp=ncompo,data=M_,segment.type="interleaved",
 validation='CV',segments=as.numeric(ans[[6]]),scale=as.logical(ans[[7]]))
 resf<-plsr(as.formula(model),ncomp=ncompo,data=M_,validation='none',
 scale=as.logical(ans[[7]]))
 rmsep<-RMSEP(res,intercep=FALSE)
 dev.new(title="PLS RMSCV and explained variance")
 op<-par(pty='s',mfrow=c(1,2))
 plot(rmsep$val[1,,],xlab='Number of Components',ylab='RMSECV',main='');grid()
 lines(rmsep$val[1,,])
 vm<-R2(res,estimate='CV',ncomp=1:ncompo,intercept=FALSE)$val[1,,]*100
 plot(vm,xlab='Number of Components',ylab='CV % Explained Variance',ylim=c(min(0,min(vm)),100));grid()#
lines(vm)
 par(op)
 print('',quote=FALSE)
 print('CV% Explained Variance',quote=FALSE)
 print(round(vm,2),quote=FALSE)
 print('',quote=FALSE)
 print('RMSECV',quote=FALSE)
 print(format(rmsep$val[1,,],digit=4),quote=FALSE)
 print('',quote=FALSE)
 print(paste('Minimum RMSECV at component n.',which.min(rmsep$val[1,,])),quote=FALSE)
 pls.set<-ans
 PLS$typ<-'PLS1'
 PLS$dataset<-M_
 PLS$naM_0 <- naM_0
 PLS$nY<-nY
 PLS$validation<-'CV'
 PLS$nseg<-as.numeric(ans[[6]])
 PLS$segtype<-'interleaved'
 PLS$scale<-as.logical(ans[[7]])
 PLS$model<-as.formula(model)
 ans<-inpboxc('Number of Components:',as.character(1:ncompo),which.min(rmsep$val[1,,])-1)
 if(!is.null(ans)){
 PLS$ncomp<-as.numeric(ans)
 pls.set[5]<-PLS$ncomp
 res<-plsr(PLS$model,ncomp=PLS$ncomp,data=PLS$dataset,segment.type="interleaved",
 validation='CV',segments=PLS$nseg,scale=PLS$scale)
 PLS$rmsep<-RMSEP(res,estimate='CV',ncomp=PLS$ncomp,intercept=FALSE)$val[1,,]
 PLS$rcv<-R2(res,estimate='CV',ncomp=PLS$ncomp,intercept=FALSE)$val[1,,]*100
 print(paste('Model created with ',format(PLS$ncomp,digits=2),
 ' components and saved in PLS object',sep=''),quote=FALSE)
 PLS$res<-res
 PLS$resf<-resf
 colnames(PLS$dataset) <- naM_0
 }
 rm(ans,M_,naM,naM_0,nNA,model,res,nY,rmsep,vm,ncompo,op,resf,Y_)}else{
tk_messageBox(type=c("ok"),message='Matrix/Table Requested !',caption="Input Error")}}
