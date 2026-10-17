# Purpose   : make the bar plot of Mahalanobis distance of a LDA object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
if(exists("CLA",envir=.GlobalEnv)){
dev.new(title="MD from closest category CV")
barplot(apply(CLA$MdCV,2,min),main='Mahalanobis Distance from the closest category (CV)',
names.arg=CLA$lb.obj,cex.names=0.5,las=2,col=CLA$group)
legend('top',legend=unique(CLA$group),col=unique(CLA$group),horiz=TRUE,pch=16)
grid();box()}else{
tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")}
