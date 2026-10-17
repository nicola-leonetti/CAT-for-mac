# Purpose   : evaluation of LDA (Linear Discriminant Analysis) using classic algorithm
# Input     : matrix with only numeric values object by row variable by column
#             factor vector with the same row of M
#             numeric value of segments to perform Cross Validation
# Output    : LDA object with
#             - original dataset
#             - group factor vecotr
#             - matrix of means
#             - matrix of covariance
# Print     : confusion matrix 
#             label of object mispredicted in CV
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 2.3
# Licence   : GPL2.1
#


#if(exists("CLA",envir=.GlobalEnv)){
require(matrixcalc)
if(!exists('QDA.pertest.set'))QDA.pertest.set<-c(previous.name,'all',' ','1','5','100')
ans<-inpboxeeeeee(c('*Matrix Name','*Rows to be selected (e.g., "1:10,15" or "15:end")',
				'*X-Variables to be selected (e.g., "1:3,7" or "3:end")',
                    '*Category Variable','*Number of Segments for CV','*Number of randomizations'),QDA.pertest.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  QDA.pertest.set<-ans
  M_<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  if((ans[[2]]!='all')& strsplit(unlist(strsplit(ans[[2]], ',')),':')[[1]][2]=='end')
    ans[[2]]<-paste(strsplit(unlist(strsplit(ans[[2]], ',')),':')[[1]][1],':',nrow(M_))
  if((ans[[3]]!='all')& strsplit(unlist(strsplit(ans[[3]], ',')),':')[[1]][2]=='end')
    ans[[3]]<-paste(strsplit(unlist(strsplit(ans[[3]], ',')),':')[[1]][1],':',ncol(M_))
	pb<-winProgressBar("computation progress bar", "Some information in %",
	                   min = 0, max = as.numeric(ans[[6]]), initial = 0, width = 300)
	D2<-as.data.frame(NULL)
	for(k in 1:as.numeric(ans[[6]])){
	  M_<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
	  if(ans[[2]]!='all')M_<-M_[givedim(ans[[2]]),]
	  a=as.numeric(Sys.time())
	  set.seed(a)
	  M_=M_[sample(nrow(M_)),]
	  gr<-factor(M_[,as.integer(ans[[4]])])
	  gr<-sample(gr)
	  M_<-M_[,givedim(ans[[3]])]
	  M_<-as.data.frame(M_)
	  vv<-ncol(M_)
	  rr<-nrow(M_)
	  ng<-as.numeric(ans[[5]])
	  lev<-levels(factor(gr))
	  cat<-nlevels(gr)
	  numcat<-summary(gr)
	  #cross validation calculation
	  d2<-matrix(rep(0,rr*cat),rr,cat)
	  me<-matrix(rep(0,vv*cat),cat,vv)
	  for(j in (1:cat)){
	    for (g in 1:ng){
	      t.seq<-seq(g,rr,by=ng)
	      Mtr<-M_[-t.seq,]
	      grt<-gr[-t.seq]
	      Mev<-M_[t.seq,]
	      co<-matrix(rep(0,vv^2),vv,vv)
	      me[j,]<-apply(Mtr[grt==lev[j],],2,mean)
	      co<-cov(Mtr[grt==lev[j],])
	      for (i in 1:length(t.seq)){
	        if(!class(try(matrix.inverse(co),silent=T))=="matrix"){
	          tk_messageBox(type=c('ok'),message=paste("Error - not enough objects!"),caption='Input Error')
	          opt <- options(show.error.messages=FALSE)
	          on.exit(options(opt))
	          stop()}
	        d2[t.seq[i],j]<-as.matrix(Mev[i,]-me[j,])%*% matrix.inverse(co)%*%t(as.matrix(Mev[i,]-me[j,]))
	      }
	    }
	  }
	dd<-t(d2)
	mt<-table(gr,apply(dd,2,which.min))
	dimnames(mt)<-list(lev,lev)
	diag(mt)<-0
	errcat<-apply(mt,1,sum)
	D2<-rbind.data.frame(D2,c(round(100*(1-(errcat/numcat)),1),round(100*(1-mean(errcat/numcat)),1)))
	#setWinProgressBar(pb, k, sprintf("computation (%s)", info), info)
	setWinProgressBar(pb, k,label = sprintf("%d%% done", round(k/as.numeric(ans[[6]])*100)))
	}
	close(pb)
	colnames(D2)<-c(lev,'% tot')
	
	print(D2) # per far vedere 
  
   


	dev.new(title="permutation test")
	plot(main='Distribution density',xlab='% tot',density(D2$`% tot`),
	     xlim=c(0,100))
	
	if(exists("CLA",envir=.GlobalEnv)){
	  M_<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
	  if(ans[[2]]!='all')M_<-M_[givedim(ans[[2]]),]
	  M_<-M_[,givedim(ans[[3]])]
	  M_<-as.data.frame(M_)
	  if(ncol(M_)==ncol(CLA$dataset) & nrow(M_)==nrow(CLA$dataset)){
	    if(sum(M_==CLA$dataset)==nrow(M_)*ncol(M_)){
	      mt<-CLA$mt
	      diag(mt)<-0
	      errcat<-apply(mt,1,sum)
	      abline(v=100*(1-mean(errcat/numcat)), col="blue")
	      print('% Total Correct Predictions in Cross Validation',quote=FALSE)
	      print(round(100*(1-mean(errcat/numcat)),1),quote=FALSE)
	      if(median(D2$`% tot`)>=100*(1-mean(errcat/numcat))){
	        print(paste0(sum(D2$`% tot`<=100*(1-mean(errcat/numcat)))/as.numeric(ans[[6]]),'%'))
	      }else{
	        print(paste0(sum(D2$`% tot`>=100*(1-mean(errcat/numcat)))/as.numeric(ans[[6]]),'%'))
	      }
	  }
	}}

	rm(numcat,cat,vv,rr,ng,errcat,me,co,mt,dd,d2,Mtr,Mev,t.seq,gr,M_,ans)
	rm_obj<-c('a','D2','g','grt','j','k','lev','pb','i')
	for (o in 1:length(rm_obj))if(exists(rm_obj[o]))rm(list=rm_obj[o])
	rm(rm_obj,o)
}
#}else{
 # tk_messageBox(type=c("ok"),message="Run LDA Classification Method first!",caption="Input Error")}
