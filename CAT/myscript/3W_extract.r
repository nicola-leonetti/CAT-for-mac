# Purpose   : extract some relevant features from a PCA3W object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.0
# Licence   : GPL2.1
#
if(exists("PCA3W",envir=.GlobalEnv)){
nl<-inpboxc('Extract Matrix:',c('Object Loadings','Variable Loadings','Conditon Loadings'))
if(!is.null(nl)){
if(nl[[1]]==1){lo<-PCA3W$lo;print('Values saved and exported in "lo"',quote=FALSE)}
if(nl[[1]]==2){lv<-PCA3W$lv;print('Values saved and exported in "lv"',quote=FALSE)}
if(nl[[1]]==3){lc<-PCA3W$lc;print('Values saved and exported in "lc"',quote=FALSE)}
}}else{tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',
caption="Input Error")}
if(nl[[1]]==1) {values="lo";col=TRUE;row=TRUE}
if(nl[[1]]==2) {values="lv";col=TRUE;row=TRUE}
if(nl[[1]]==3) {values="lc";col=TRUE;row=TRUE}
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
