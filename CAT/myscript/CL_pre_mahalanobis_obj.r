# Purpose   : make the bar plot of Mahalanobis distance of a LDA object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.0
# Licence   : GPL2.1
#
if(exists("CLA",envir=.GlobalEnv)){
if(!is.null(CLA$Md.pre)){
	ans<-inpboxc('Object',CLA$lb.obj.pre,inp=-1)
	if(!is.null(ans)){
                dev.new(title="MD from predefined object pred")
		barplot(CLA$Md.pre[,as.numeric(ans[[1]])],main=paste0('Mahalanobis Distance - ',
		                                                      CLA$lb.obj.pre[as.numeric(ans)]),
		names.arg=CLA$lev,cex.names=1)
	grid();box()
	}
}else{
tk_messageBox(type=c("ok"),message="Run Prediction first!",caption="Input Error")}	
}else{
tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")}
