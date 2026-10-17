# Purpose   : evaluate the fitted value in Cross-Validation from a PLS object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.7
# Licence   : GPL2.1
#
if(exists("PLS",envir=.GlobalEnv)){
if(PLS$typ!='PLS2'){

ans<-inpboxe4k1(c('*Matrix Name with samples to be predicted','*Rows (e.g., "1:10,15" or "15:end")','*X-Variables (e.g., "1:3,7" or "3:end")',
'Y-Variable (e.g., 9)'),c('Row Names'),c(previous.name,'all','all','None','FALSE'))
if(!is.null(ans)){
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

 loY<-TRUE
 if(ans[[4]]!='None'){
   if(is.na(suppressWarnings(as.numeric(ans[[4]])))){
     Y_<-eval(parse(text=ans[[4]]),envir=.GlobalEnv)
   }else{
     Y_<-M_[,as.numeric(ans[[4]])] 
     if(ans[[2]]!='all')Y_<-Y_[givedim(ans[[2]])]
   }
 }else{
  loY<-FALSE
	Y_<-rep(0,nrow(M_))
	if(ans[[2]]!='all')Y_<-Y_[givedim(ans[[2]])]
 }
 if((ans[[2]]!='all')&(ans[[3]]!='all'))M_<-M_[givedim(ans[[2]]),givedim(ans[[3]])]
 if((ans[[2]]!='all')&(ans[[3]]=='all'))M_<-M_[givedim(ans[[2]]),]
 if((ans[[2]]=='all')&(ans[[3]]!='all'))M_<-M_[,givedim(ans[[3]])]
 M_<-cbind.data.frame(Y_,M_)
 if(ans[[4]]=='None')colnames(M_)[1] <- colnames(PLS$dataset)[1]
 if(ans[[4]]!='None')colnames(M_)[1]<-colnames(eval(parse(text=ans[[1]]),envir=.GlobalEnv))[as.numeric(ans[[4]])]
 M_ <- data.frame(M_)
 prm<-drop(predict(PLS$res,newdata=M_,ncomp=1:PLS$ncomp,scale=PLS$scale))
 prm.tr<-drop(predict(PLS$res,newdata=NULL,ncomp=1:PLS$ncomp,scale=PLS$scale))
 rmsep<-RMSEP(PLS$res,estimate='test',newdata=M_,ncomp=PLS$ncomp,scale=PLS$scale,
 intercept=FALSE)$val[1,,]
 if(nrow(M_)==1)prm<-matrix(unlist(prm),1,PLS$ncomp)
 res<-prm[,PLS$ncomp]-Y_
 res.tr<-prm.tr[,PLS$ncomp]-PLS$dataset[,1]
 if(loY){
    print('Prediction Statistics',quote=FALSE)
    print('',quote=FALSE)
    print(paste('RMSEP:',format(rmsep,digits=4)),quote=FALSE)
    print(paste('BIAS :',format(mean(res),digits=4)),quote=FALSE)
    print('',quote=FALSE)
    dev.new(title="calibration predictions")
    op<-par(pty='s',mfrow=c(1,2))
    if(!as.logical(ans[[5]])){
           plot(Y_,prm[,PLS$ncomp],xlab='Experimental Value',ylab='Predicted Value',asp=1,
           xlim=c(min(c(Y_,prm[,PLS$ncomp],prm.tr[,PLS$ncomp])),
		          max(c(Y_,prm[,PLS$ncomp],prm.tr[,PLS$ncomp]))),
           ylim=c(min(c(Y_,prm[,PLS$ncomp],prm.tr[,PLS$ncomp])),
		          max(c(Y_,prm[,PLS$ncomp],prm.tr[,PLS$ncomp]))))
           lines(par('usr')[1:2],par('usr')[3:4],col='red')
		   grid()
           plot(1:nrow(M_),res,xlab='Object Number',ylab='Residuals',
		   ylim=c(min(min(res),min(res.tr)),max(c(res,res.tr))))
           abline(h=0,col="red")
		   grid()
	}else{
           plot(Y_,prm[,PLS$ncomp],xlab='Experimental Value',ylab='Predicted Value',asp=1,
           xlim=c(min(c(Y_,prm[,PLS$ncomp],prm.tr[,PLS$ncomp])),
		          max(c(Y_,prm[,PLS$ncomp],prm.tr[,PLS$ncomp]))),
           ylim=c(min(c(Y_,prm[,PLS$ncomp],prm.tr[,PLS$ncomp])),
		          max(c(Y_,prm[,PLS$ncomp],prm.tr[,PLS$ncomp]))),type='n')
           text(Y_,prm[,PLS$ncomp],as.character(row.names(M_)))
           lines(par('usr')[1:2],par('usr')[3:4],col='red')
		   grid()
           plot(1:nrow(M_),res,xlab='Object Number',ylab='Residuals',type='n',
		   ylim=c(min(c(res,res.tr)),max(c(res,res.tr))))
           text(1:nrow(M_),res,as.character(row.names(M_)))
           abline(h=0,col="red")
		   grid()
	}
    par(op)
    rm(op)
 }
 print('Predicted Values',quote=FALSE)
 print(prm[,PLS$ncomp])
 assign('var.pred',prm[,PLS$ncomp],envir=.GlobalEnv)
 print('Predicted values saved in "var.pred"',quote=FALSE)
 rm(prm,res,M_,Y_,rmsep,prm.tr,res.tr,loY)}
 rm(ans)
}else{tk_messageBox(type=c("ok"),message='Not implemented for PLS2',caption="Input Error")}
}else{tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCR/PLS1/PLS2!',
 caption="Input Error")}
