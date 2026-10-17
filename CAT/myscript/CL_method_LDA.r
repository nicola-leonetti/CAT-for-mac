# Purpose   : LDA (Linear Discriminant Analysis) using classic algorithm
# Output    : LDA object with
#             - original dataset
#             - group factor vecotr
#             - matrix of means
#             - matrix of covariance
# Print     : confusion matrix 
#             label of objects mispredicted in CV
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 2.3
# Licence   : GPL2.1
#
require(matrixcalc)
if(!exists('LDA.set'))LDA.set<-c(previous.name,'all','','1','5')
ans<-inpboxeeeee(c('*Matrix Name','*Rows (e.g., "1:10,15" or "15:end")','*X-Variables (e.g., "1:3,7" or "3:end")',
                   '*Category Variable','*Number of Segments for CV'),LDA.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  LDA.set<-ans
	CLA<-list()
	M_<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
        if((ans[[2]]!='all')& strsplit(unlist(strsplit(ans[[2]], ',')),':')[[1]][2]=='end')
         ans[[2]]<-paste(strsplit(unlist(strsplit(ans[[2]], ',')),':')[[1]][1],':',nrow(M_))
        if((ans[[3]]!='all')& strsplit(unlist(strsplit(ans[[3]], ',')),':')[[1]][2]=='end')
         ans[[3]]<-paste(strsplit(unlist(strsplit(ans[[3]], ',')),':')[[1]][1],':',ncol(M_))
	if(ans[[2]]!='all')M_<-M_[givedim(ans[[2]]),]
	gr<-factor(M_[,as.integer(ans[[4]])])
	M_<-M_[,givedim(ans[[3]])]
	CLA$dataset<-M_
	M_<-as.data.frame(M_)
	vv<-ncol(M_)
	rr<-nrow(M_)
	ng<-as.numeric(ans[[5]])
	lev<-levels(factor(gr))
	cat<-nlevels(gr)
	numcat<-summary(gr)
	co<-matrix(rep(0,vv^2),vv,vv)
	me<-matrix(rep(0,vv*cat),cat,vv)
	for (i in 1:cat){
	  me[i,]<-apply(M_[gr==lev[i],],2,mean)
	  co<-co+cov(M_[gr==lev[i],])*(sum(gr==lev[i])-1)/(rr-cat)
	}
	CLA$means<-me
	CLA$cov<-co
	CLA$dataset<-M_
	CLA$group<-gr
	CLA$lev<-lev
	CLA$lb.obj<-unlist(dimnames(CLA$dataset)[1])
	if(is.null(CLA$lb.obj))CLA$lb.obj<-as.character(1:rr)
	#cross validation calculation
	d2<-matrix(rep(0,rr*cat),rr,cat)
	for (g in 1:ng){
		t.seq<-seq(g,rr,by=ng)
		Mtr<-M_[-t.seq,]
		grt<-gr[-t.seq]
		Mev<-M_[t.seq,]
		co<-matrix(rep(0,vv^2),vv,vv)
		me<-matrix(rep(0,vv*cat),cat,vv)
		for (i in 1:cat){
			me[i,]<-apply(Mtr[grt==lev[i],],2,mean)
			co<-co+cov(Mtr[grt==lev[i],])*(sum(grt==lev[i])-1)/(nrow(Mtr)-cat)
		}
		for (i in 1:length(t.seq)){
			for (j in 1:cat){
			  if(!class(try(matrix.inverse(co),silent=T))=="matrix"){
			    if(nrow(Mtr)< vv){
			      tk_messageBox(type=c('ok'),message=paste("Error - not enough objects!"),caption='Input Error')
			    }else{
			      tk_messageBox(type=c('ok'),message=paste("LDA cannot be executed due to the correlation structure of the data matrix."),caption='Input Error')
			    }
			    opt <- options(show.error.messages=FALSE)
			    on.exit(options(opt))
			    stop()}
			  d2[t.seq[i],j]<-as.matrix(Mev[i,]-me[j,])%*%matrix.inverse(co)%*%t(as.matrix(Mev[i,]-me[j,]))
			}
		}
	}
	dd<-t(d2)
	CLA$MdCV<-dd
	print('Confusion Matrix in Cross Validation',quote=FALSE)
	mt<-table(gr,apply(dd,2,which.min))
	dimnames(mt)<-list(lev,lev)
	CLA$mt<-mt
	print(mt,quote=FALSE)
	diag(mt)<-0
	errcat<-apply(mt,1,sum)
	if(sum(errcat)!=0){
		print('Labels of Samples with wrong assignment',quote=FALSE)
		print(row.names(M_)[which(lev[apply(dd,2,which.min)]!=gr)],quote=FALSE)
	}
	print('% Correct Predictions in Cross Validation',quote=FALSE)
	print(round(100*(1-(errcat/numcat)),1),quote=FALSE)
	print('% Total Correct Predictions in Cross Validation',quote=FALSE)
	print(round(100*(1-mean(errcat/numcat)),1),quote=FALSE)
	print('Note: Data are saved in the CLA object, write CLA to see all',quote=FALSE)
	rm(numcat,cat,vv,rr,ng,errcat,me,co,mt,dd,d2,Mtr,Mev,t.seq,gr,M_,ans)
}
