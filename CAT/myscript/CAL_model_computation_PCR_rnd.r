# Purpose   : repeat n PCR models each time with a different composition of the deletion groups  
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: CM
# Version   : 2.3
# Licence   : GPL2.1
#
suppressWarnings(suppressPackageStartupMessages(library("Rcpp")))
library(pls)
library(gplots)
require(pcaMethods)
if(!exists('pls_rnd.set'))pls_rnd.set<-c(previous.name,'all','all','None','10','5','100',TRUE)
ans<-inpboxe7k1(c('*Matrix Name','*Rows (e.g., "1:10,15" or "15:end")',
'*X-Variables (e.g., "1:3,7" or "3:end")','*Y-Variable (e.g., 9)',
'*Number of Components','*Number of Segments for CV','*Number of randomizations'),'Scaled (centering by default)',pls_rnd.set)
if(!is.null(ans)){
 PLS<-list()
 previous.name<-ans[[1]]
 sc<-as.logical(ans[[8]])
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
 M_ <- data.frame(M_)
 naM<-names(M_)
 nNA<-sum(is.na(M_))
 nY<-1
 if(nNA>0){
 mess<-paste(as.character(nNA),'NA present.We try to rebuild them!')
 tk_messageBox(type=c("ok"),message=mess,caption="Input Error")
 md<-prep(M_,scale="uv",center=TRUE,simple=FALSE,rev=FALSE)
 res<-pca(md$data,method="nipals",nPcs=min(ncol(M_),10),scale="uv",center=TRUE)
 M_<-prep(res@completeObs,scale=md$scale,center=md$center,reverse=TRUE)
 M_<-as.data.frame(M_)
 rm(md,res)}
 
 M._<-M_
 ncompo<-min(as.numeric(ans[[5]]),ncol(M_)-1)
 model<-paste(naM[nY],'~',(paste(naM[-nY],collapse='+')),sep='')
 
 N<-c(NULL)
 D<-data.frame(NULL)
 
 pb<-winProgressBar("computation progress bar", "Some information in %",
                    min = 0, max = as.numeric(ans[[7]]), initial = 0, width = 300)
 for(i in 1:as.numeric(ans[[7]])){
   a=as.numeric(Sys.time())
   set.seed(a)
   M_=M_[sample(nrow(M_)),]
   M_<-as.data.frame(M_)
   res<-pcr(as.formula(model),ncomp=ncompo,data=M_,segment.type="interleaved",
            validation='CV',segments=as.numeric(ans[[6]]),scale=as.logical(ans[[8]]))
   #resf<-pcr(as.formula(model),ncomp=ncompo,data=M_,validation='none',
   #scale=as.logical(ans[[7]]))
   rmsep<-RMSEP(res,intercep=FALSE)
   N[i]<-which.min(rmsep$val[1,,])
   D<-rbind.data.frame(D,rmsep$val[1,,])
   setWinProgressBar(pb,i,label=sprintf("%d%% done", round(i/as.numeric(ans[[7]])*100)))
 }
 
 close(pb)
 
 colnames(D)<-paste('Comp',c(1:ans[[5]]))
 D<-cbind.data.frame(N=N,D)
 D_min<-apply(D[,-1],2,min)
 D_max<-apply(D[,-1],2,max)
 
 R_sq<-1-apply(D[,-1]^2,2,mean)*length(Y_)/sum((Y_ - mean(Y_))^2)
 
 
 print('',quote=FALSE)
 print('CV% Explained Variance',quote=FALSE)
 print(round(R_sq*100,2),quote=FALSE)
 print('',quote=FALSE)
 print('Global RMSECV',quote=FALSE)
 print(round(sqrt(apply(D^2,2,mean))[-1],4))
 print('',quote=FALSE)
 print(paste('Minimum Global RMSECV at component n.',which.min(sqrt(apply(D^2,2,mean))[-1])))
 
 dev.new(title="PCR RMSCV and explained variance") 
 op<-par(pty='s',mfrow=c(2,2))
 plot(sqrt(apply(D^2,2,mean))[-1],xlab='Number of Components',ylab='Global RMSECV',main='',
      ylim = c(min(D_min),max(D_max)));grid()
 lines(sqrt(apply(D^2,2,mean))[-1]) 
 lines(D_min,col='red',lty = 2)
 lines(D_max,col='red',lty = 2)
 
 plot(R_sq*100,xlab='Number of Components',ylab='CV % Explained Variance',
      ylim=c(min(0,min(R_sq*100)),100));grid()
 lines(R_sq*100)
 
 N<-c(N,min(N):max(N))
 F<-as.factor(N)
 plot(F[1:ans[[7]]],xlab='Number of Components',ylab='Frequency')
 par(op)
 
 
 pls_rnd.set<-ans
 PLS$typ<-'PCR'
 PLS$dataset<-M._
 PLS$naM_0 <- naM_0
 PLS$nY<-nY
 PLS$validation<-'CV'
 PLS$nseg<-as.integer(ans[[6]])
 PLS$segtype<-'interleaved'
 PLS$scale<-sc
 PLS$model<-as.formula(model)
 
 ans<-inpboxc('Number of Components:',as.character(1:ncompo),which.min(rmsep$val[1,,])-1)
 if(!is.null(ans)){
   PLS$ncomp<-as.numeric(ans)
   pls_rnd.set[5]<-PLS$ncomp
   res<-pcr(PLS$model,ncomp=PLS$ncomp,data=PLS$dataset,segment.type="interleaved",
             validation='CV',segments=PLS$nseg,scale=PLS$scale)
   resf<-pcr(PLS$model,ncomp=PLS$ncomp,data=PLS$dataset,validation='none',
              scale=sc)
   PLS$rmsep<-RMSEP(res,estimate='CV',ncomp=PLS$ncomp,intercept=FALSE)$val[1,,]
   PLS$rcv<-R2(res,estimate='CV',ncomp=PLS$ncomp,intercept=FALSE)$val[1,,]*100
   print(paste('Model created with ',format(PLS$ncomp,digits=2),
               'components and saved in PLS object',sep=''),quote=FALSE)
   PLS$res<-res
   PLS$resf<-resf
   colnames(PLS$dataset) <- naM_0
 }
 
 if(exists('a'))rm(a)
 if(exists('D'))rm(D)
 if(exists('D_min'))rm(D_min)
 if(exists('D_max'))rm(D_max)
 if(exists('F'))rm(F)
 if(exists('i'))rm(i)
 if(exists('M._'))rm(M._)
 if(exists('M_'))rm(M_)
 if(exists('N'))rm(N)
 if(exists('naM'))rm(naM)
 if(exists('naM_0'))rm(naM_0)
 if(exists('ncompo'))rm(ncompo)
 if(exists('nNA'))rm(nNA)
 if(exists('nY'))rm(nY)
 if(exists('op'))rm(op)
 if(exists('res'))rm(res)
 if(exists('resf'))rm(resf)
 if(exists('rmsep'))rm(rmsep)
 if(exists('Y_'))rm(Y_)
 if(exists('sc'))rm(sc)
 if(exists('R_sq'))rm(R_sq)}else{
tk_messageBox(type=c("ok"),message='Matrix/Table Requested !',caption="Input Error")}}
