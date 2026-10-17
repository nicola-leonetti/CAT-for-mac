# Purpose   : Bar Plot of Objects RMSE from a PCA3W object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.0
# Licence   : GPL2.1
#
if(exists("PCA3W",envir=.GlobalEnv)){
c<-PCA3W$limits[1]
o<-PCA3W$limits[2]
v<-PCA3W$limits[3]
dev.new(title="RMSE objects")
ro<-rep(0,o)
roc<-apply(PCA3W$dis,1,sum)
for(i in 1:o)ro[i]<-sum(roc[seq(i,o*c,o)])
ro<-ro/(v*c)
barplot(ro,col='red',main='RMSE of Objects',ylim=c(0,1.2*max(ro)),names.arg=as.character(1:o),cex.names=0.6);box()
rm(c,o,v,ro,roc)
}else{tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',
caption="Input Error")}
