# Purpose   : make the bar plot of Mahalanobis distance of a LDA object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
if(exists("CLA",envir=.GlobalEnv)){
	ans<-inpboxc('Category',CLA$lev,inp=-1)
	if(!is.null(ans)){
                dev.new(title="MD from predefined category CV")
		barplot(CLA$MdCV[as.numeric(ans[[1]]),],main=paste0('Mahalanobis Distance (Cross Validation) - '
		                                                    ,CLA$lev[as.numeric(ans[[1]])]),
		names.arg=CLA$lb.obj,cex.names=0.5,las=2,col=CLA$group)
		legend('top',legend=unique(CLA$group),col=unique(CLA$group),horiz=TRUE,pch=16)
		grid();box()
	}	
}else{
tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")}
