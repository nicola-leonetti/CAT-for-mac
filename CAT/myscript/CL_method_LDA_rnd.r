# Purpose   : evaluation of the stability of the LDA results in CV
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: CM
# Version   : 2.3
# Licence   : GPL2.1
#
require(matrixcalc)
if(!exists('LDA.rnd.set'))LDA.rnd.set<-c(previous.name,'all','','1','5','100')
ans<-inpboxeeeeee(c('*Matrix Name','*Rows (e.g., "1:10,15" or "15:end")','*X-Variables (e.g., "1:3,7" or "3:end")',
                    '*Category Variable','*Number of Segments for CV','*Number of randomizations'),LDA.rnd.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  LDA.rnd.set<-ans
  CLA_RND<-list()
	pb<-winProgressBar("computation progress", "% done",
	                   min = 0, max = as.numeric(ans[[6]]), initial = 0, width = 300)

	M_<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)

        if((ans[[2]]!='all')& strsplit(unlist(strsplit(ans[[2]], ',')),':')[[1]][2]=='end')
         ans[[2]]<-paste(strsplit(unlist(strsplit(ans[[2]], ',')),':')[[1]][1],':',nrow(M_))
        if((ans[[3]]!='all')& strsplit(unlist(strsplit(ans[[3]], ',')),':')[[1]][2]=='end')
         ans[[3]]<-paste(strsplit(unlist(strsplit(ans[[3]], ',')),':')[[1]][1],':',ncol(M_))

	if(ans[[2]]!='all')M_<-M_[givedim(ans[[2]]),]
	gr<-factor(M_[,as.integer(ans[[4]])])
	M_<-M_[,givedim(ans[[3]])]
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
	CLA_RND$means<-me
	CLA_RND$cov<-co
	CLA_RND$dataset<-M_
	CLA_RND$group<-gr
	CLA_RND$lev<-lev
	CLA_RND$lb.obj<-unlist(dimnames(CLA_RND$dataset)[1])
	if(is.null(CLA_RND$lb.obj))CLA_RND$lb.obj<-as.character(1:rr)
	rn_0<-rownames(M_)
	D2<-as.data.frame(NULL)
	CM<-array(rep(0,as.numeric(cat)*as.numeric(cat)*as.numeric(ans[[6]])),c(cat,cat,as.numeric(ans[[6]])))
	CMM<-matrix(rep(0,cat*cat),ncol=cat)
	OM<-matrix(rep(0,nrow(M_)*as.numeric(ans[[6]])),nrow=nrow(M_))
	for(k in 1:as.numeric(ans[[6]])){
	  M_<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
	  if(ans[[2]]!='all')M_<-M_[givedim(ans[[2]]),]
	  a=as.numeric(Sys.time())
	  set.seed(a)
	  M_=M_[sample(nrow(M_)),]
	  gr<-factor(M_[,as.integer(ans[[4]])])
	  M_<-M_[,givedim(ans[[3]])]
	  M_<-as.data.frame(M_)
	  rn<-rownames(M_)
	  lev<-levels(factor(gr))
	  cat<-nlevels(gr)
	  numcat<-summary(gr)
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
			    tk_messageBox(type=c('ok'),message=paste("Error - not enough objects!"),caption='Input Error')
			    opt <- options(show.error.messages=FALSE)
			    on.exit(options(opt))
			    stop()}
			  d2[t.seq[i],j]<-as.matrix(Mev[i,]-me[j,])%*%matrix.inverse(co)%*%t(as.matrix(Mev[i,]-me[j,]))
			}
		}
	}
	dd<-t(d2)
	mt<-table(gr,apply(dd,2,which.min))
	dimnames(mt)<-list(lev,lev)
	CM[,,k]<-mt
	CMM<-CMM+mt
	X<-matrix(lev[matrix(apply(dd,2,which.min),ncol=1)],ncol=1)
	rownames(X)<-rn
	X<-matrix(X[rn_0,],ncol=1)
	OM[,k]<-X
	diag(mt)<-0
	errcat<-apply(mt,1,sum)
	D2<-rbind.data.frame(D2,c(100*(1-(errcat/numcat)),100*(1-mean(errcat/numcat))))
	setWinProgressBar(pb, k,label = sprintf("%d%% done", round(k/as.numeric(ans[[6]])*100)))
	}
	
	close(pb)
	colnames(D2)<-c(lev,'% tot')

	for(k in 1:cat){
	  if(nrow(unique(D2[k]))>1){
	    dev.new(title=paste("Histogram cat.",colnames(D2)[k]))
	    plot(main=colnames(D2)[k],hist(D2[,k]),xlab='',col="grey")
	  }else{
	    dev.new(title=paste("Barplot cat.",colnames(D2)[k]))
	    if(unique(D2[k])<100&unique(D2[k])>0)barplot(table(factor(D2[,k],levels=c(0,unique(D2[k]),100))),main=colnames(D2)[k])
	    if(unique(D2[k])==100)barplot(table(factor(D2[,k],levels=c(0,50,unique(D2[k])))),main=colnames(D2)[k])
	    if(unique(D2[k])==0)barplot(table(factor(D2[,k],levels=c(unique(D2[k]),50,100))),main=colnames(D2)[k])
	  }
	}
  if(nrow(unique(D2[cat+1]))>1){
    dev.new(title="Histogram total")
    plot(main="Total",hist(D2[,cat+1]),xlab='',col="grey")
  }else{
    dev.new(title="Barplot total")
    if(unique(D2[cat+1])<100&unique(D2[cat+1])>0)barplot(table(factor(D2[,cat+1],levels=c(0,unique(D2[cat+1]),100))),main="Total")
    if(unique(D2[cat+1])==100)barplot(table(factor(D2[,cat+1],levels=c(0,50,unique(D2[cat+1])))),main="Total")
    if(unique(D2[cat+1])==0)barplot(table(factor(D2[,cat+1],levels=c(unique(D2[cat+1]),50,100))),main="Total")
  }

  print('',quote=FALSE)
  print('',quote=FALSE)  
  D3<-cbind.data.frame(apply(D2,2,'mean'),apply(D2,2,'min'),apply(D2,2,'max'))  
  rownames(D3)<-c(lev,'total')
  colnames(D3)<-c('mean','min','max')
  print('% of correct predictions',quote=FALSE)
  print(format(round(D3,2),nsmall=2))
  print('',quote=FALSE)
  print('Average confusion matrix',quote=FALSE)
  print(as.table(format(round(CMM/as.numeric(ans[[6]]),2),nsmall=2)))
  rownames(OM)<-rn_0

  CLA_RND$cm<-CM   # conf. matrix
  CLA_RND$pm<-D2   # % matrix
  CLA_RND$pm_summary<-D3  # mean, min, max % matrix
  CLA_RND$om<-OM # Obj classification matrix

	rm(numcat,cat,vv,rr,ng,errcat,me,co,mt,dd,d2,Mtr,Mev,t.seq,gr,M_,ans)
	rm_obj<-c('a','D2','g','grt','j','k','lev','pb','i','CM','CMM','D3','OM',
	          'rn','rn_0','X')
	for (o in 1:length(rm_obj))if(exists(rm_obj[o]))rm(list=rm_obj[o])
	rm(rm_obj,o)
}

if(!exists("CLA",envir=.GlobalEnv)) CLA <- CLA_RND
