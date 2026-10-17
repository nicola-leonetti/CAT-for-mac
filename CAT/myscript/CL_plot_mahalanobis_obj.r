# Purpose   : make the bar plot of Mahalanobis distance of a LDA object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
if(exists("CLA",envir=.GlobalEnv)){
	ans<-inpboxc('Object',CLA$lb.obj,inp=-1)
	if(!is.null(ans)){
                dev.new(title="MD from predefined object CV")
		barplot(CLA$MdCV[,as.numeric(ans[[1]])],main=paste0('Mahalanobis Distance (Cross Validation) - ',
		                                                    CLA$lb.obj[as.numeric(ans)]),
		names.arg=CLA$lev,cex.names=1)
	grid();box()
	}
}else{
tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")}
