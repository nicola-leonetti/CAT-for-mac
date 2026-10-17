# Purpose   : extract some relevant features from a LDA object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.2
# Licence   : GPL2.1
#
if(exists("CLA",envir=.GlobalEnv)){
nl<-inpboxc('*Extract Matrix:',c('Means','Covariance','Mahalanobis Dist. in CV','Prediction'))
if(!is.null(nl)){
if(nl[[1]]==1){Mea<-CLA$means;print('Values saved and exported in "Mea"',quote=FALSE)}
if(nl[[1]]==2){Cov<-CLA$cov;print('Values saved and exported in "Cov"',quote=FALSE)}
if(nl[[1]]==3){MCV<-t(CLA$MdCV);colnames(MCV)<-CLA$lev;rownames(MCV)<-CLA$lb.obj;print('Values saved and exported in "MCV"',quote=FALSE)}
if((nl[[1]]==4)&(!is.null(CLA$Md.pre))){Pre<-t(CLA$Md.pre);colnames(Pre)<-CLA$lev;rownames(Pre)<-CLA$lb.obj.pre;print('Mahalanobis distances saved and exported in "Pre"',quote=FALSE)}
if((nl[[1]]==4)&(is.null(CLA$Md.pre))){tk_messageBox(type=c("ok"),message="Run Prediction first!",caption="Input Error")}}
}else{
tk_messageBox(type=c("ok"),message="Run one Classification Method first!",
caption="Input Error")}

if(nl[[1]]==1) {values="Mea";col=TRUE;row=TRUE}
if(nl[[1]]==2) {values="Cov";col=TRUE;row=TRUE}
if(nl[[1]]==3) {values="MCV";col=TRUE;row=TRUE}
if(nl[[1]]==4&(!is.null(CLA$Md.pre))) {values="Pre";col=TRUE;row=TRUE}

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

