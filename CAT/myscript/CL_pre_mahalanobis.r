# Purpose   : make the bar plot of Mahalanobis distance of a prediction
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
if(exists("CLA",envir=.GlobalEnv)){
if(!is.null(CLA$Md.pre)){
        dev.new(title="MD from closest category pred")
	barplot(apply(CLA$Md.pre,2,min),main='Mahalanobis Distance from the closest category',
	names.arg=CLA$lb.obj.pre,cex.names=0.5,las=2,col=CLA$pre.group)
	if(!is.null(CLA$pre.group))legend('top',legend=unique(CLA$pre.group),col=unique(CLA$pre.group),horiz=TRUE,pch=16)
	grid();box()
}else{
tk_messageBox(type=c("ok"),message="Run Prediction first!",caption="Input Error")}
}else{
tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")}
