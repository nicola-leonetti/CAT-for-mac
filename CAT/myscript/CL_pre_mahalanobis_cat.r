# Purpose   : make the bar plot of Mahalanobis distance of a LDA object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.0
# Licence   : GPL2.1
#
if(exists("CLA",envir=.GlobalEnv)){
if(!is.null(CLA$Md.pre)){
	ans<-inpboxc('Category',CLA$lev,inp=-1)
	if(!is.null(ans)){
                dev.new(title="MD from predefined category pred")
		barplot(CLA$Md.pre[as.numeric(ans[[1]]),],main=paste0('Mahalanobis Distance - '
		                                                      ,CLA$lev[as.numeric(ans[[1]])]),
		names.arg=CLA$lb.obj.pre,cex.names=0.5,las=2,col=CLA$pre.group)
		if(!is.null(CLA$pre.group))legend('top',legend=unique(CLA$pre.group),col=unique(CLA$pre.group),horiz=TRUE,pch=16)
		grid();box()
	}
}else{
tk_messageBox(type=c("ok"),message="Run Prediction first!",caption="Input Error")}	
}else{
tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")}
