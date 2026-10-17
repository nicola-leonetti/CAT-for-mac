# Purpose   : prediction of test objects
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 2.2
# Licence   : GPL2.1
#
require(matrixcalc)
if(exists("CLA",envir=.GlobalEnv)){
if(!exists('QDA.pred.set'))QDA.pred.set<-c(previous.name,'all','','None')  
ans<-inpboxeeee(c('*Matrix with samples to be predicted',
				'*Rows (e.g., "1:10,15" or "15:end")',
				'*X-Variables (e.g., "1:3,7" or "3:end")',
				'Category Variable'),QDA.pred.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  QDA.pred.set<-ans
	Mtest<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
	Mtest<-as.data.frame(Mtest)
	cat<-length(CLA$lev)
	numcat<-NULL
	gr<-NULL
	CLA$pre.group <- gr
	if(ans[[4]]!='None'){
	  gr<-Mtest[,as.integer(ans[[4]])]
	  for (i in 1:cat){
	    numcat[i]<-sum(gr==CLA$lev[i])
	  }
	  CLA$pre.group <- as.factor(gr)
	}
		.a <- strsplit(unlist(strsplit(ans[[2]], ',')),':')
	if((ans[[2]]!='all')& .a[[length(.a)]][length(.a[[length(.a)]])]=='end'){
	  .a[[length(.a)]][2]=nrow(Mtest)
	  for(i in 1:length(.a)).a[[i]]<-paste(.a[[i]],collapse=':')
	  ans[[2]]<-paste(unlist(.a),collapse=',')
	}
	.a <- strsplit(unlist(strsplit(ans[[3]], ',')),':')
	if((ans[[3]]!='all')& .a[[length(.a)]][length(.a[[length(.a)]])]=='end'){
	  .a[[length(.a)]][2]=ncol(Mtest)
	  for(i in 1:length(.a)).a[[i]]<-paste(.a[[i]],collapse=':')
	  ans[[3]]<-paste(unlist(.a),collapse=',')
	}
	rm(.a)

	if((ans[[2]]!='all')&(ans[[3]]!='all'))Mtest<-Mtest[givedim(ans[[2]]),givedim(ans[[3]])]
	if((ans[[2]]!='all')&(ans[[3]]=='all'))Mtest<-Mtest[givedim(ans[[2]]),]
	if((ans[[2]]=='all')&(ans[[3]]!='all'))Mtest<-Mtest[,givedim(ans[[3]])]

	vv<-ncol(Mtest)
	rrt<-nrow(Mtest)
	M_<-CLA$dataset
	rr<-nrow(M_)
	cc<-ncol(M_)
	group<-CLA$group
	numcat<-NULL
	if(!is.null(gr)){
		for (i in 1:cat){
			numcat[i]<-sum(gr==CLA$lev[i])
		}
	}
	d2test<-matrix(rep(0,rrt*cat),rrt,cat)
	co<-matrix(rep(0,vv^2),vv,vv)
	me<-matrix(rep(0,vv*cat),cat,vv)
	for(j in 1:cat){
		me[j,]<-apply(M_[group==CLA$lev[j],],2,mean)
		co<-cov(M_[group==CLA$lev[j],])
		for(i in 1:rrt){
			d2test[i,j]<-as.matrix(Mtest[i,]-me[j,])%*% matrix.inverse(co)%*%t(as.matrix(Mtest[i,]-me[j,]))
		}
	}
	dd<-t(d2test)
	CLA$Md.pre<-dd
	CLA$lb.obj.pre<-unlist(dimnames(Mtest)[1])
	if(!is.null(gr)){ #test prediction
		print('Confusion Matrix in Prediction',quote=FALSE)
		grt<-CLA$lev[apply(dd,2,which.min)]
		CLA$pred <- as.factor(grt)
		mt<-matrix(rep(0,cat^2),cat,cat)
		mt<-as.data.frame(mt)
		names(mt)<-CLA$lev
		rownames(mt)<-CLA$lev
		for(i in 1:cat){
			for(j in 1:cat){
				for(k in 1:rrt){
					if((CLA$lev[i]==gr[k])&(CLA$lev[j]==grt[k]))mt[i,j]<-mt[i,j]+1
				}
			}
		}
		print(mt,quote=FALSE)
		diag(mt)<-0
		errcat<-apply(mt,1,sum)
		numcat<-numcat[which(names(errcat)%in%levels(as.factor(gr)))]
		errcat<-errcat[names(errcat)%in%levels(as.factor(gr))]
		print('Labels of Samples with wrong assignment',quote=FALSE)
		print(row.names(Mtest)[which(CLA$lev[apply(dd,2,which.min)]!=gr)],quote=FALSE)
		print('% Correct Predictions',quote=FALSE)
		errcat<-errcat/numcat
		errcat[is.nan(errcat)]<-0
		print(round(100*(1-errcat),1),quote=FALSE)
		print('% Total Correct Predictions',quote=FALSE)
		print(round(100*(1-mean(errcat)),1),quote=FALSE)
		rm(errcat,numcat)
	}else{ #pure prediction
		print('Category Prediction',quote=FALSE)
		mt<-CLA$lev[apply(dd,2,which.min)]
		CLA$pred <- as.factor(mt)
		mt<-matrix(mt,length(mt),1)
		dimnames(mt)<-list(rownames(Mtest),'')
		print(mt,quote=FALSE)
	}
	rm(dd,mt,cc,rr,rrt,vv,group,me,co,d2test,cat,gr,Mtest)
  rm(i,j,M_)
  if(exists('grt'))rm(grt)
  if(exists('k'))rm(k)
  if(exists('numcat'))rm(numcat)
}}else{
tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")}
