# Purpose   : Bar Plot of Variables RMSE from a PCA3W object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.0
# Licence   : GPL2.1
#
if(exists("PCA3W",envir=.GlobalEnv)){
c<-PCA3W$limits[1]
o<-PCA3W$limits[2]
v<-PCA3W$limits[3]
dev.new(title="RMSE variables")
rv<-rep(0,v)
rv<-apply(PCA3W$dis,2,sum)
rv<-rv/(o*c)
barplot(rv,col='green',main='RMSE of Variables',ylim=c(0,1.2*max(rv)),names.arg=as.character(1:v),cex.names=1.0);box()
rm(c,o,v,rv)
}else{tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',
caption="Input Error")}
