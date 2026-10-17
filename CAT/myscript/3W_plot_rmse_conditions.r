# Purpose   : Bar Plot of Conditions RMSE from a PCA3W object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.0
# Licence   : GPL2.1
#
if(exists("PCA3W",envir=.GlobalEnv)){
c<-PCA3W$limits[1]
o<-PCA3W$limits[2]
v<-PCA3W$limits[3]
dev.new(title="RMSE conditions")
rc<-rep(0,c)
roc<-apply(PCA3W$dis,1,sum)
for(i in 1:c)rc[i]<-sum(roc[((i-1)*o+1):(i*o)]);
rc<-rc/(o*v)
barplot(rc,col='blue',main='RMSE of Conditions',ylim=c(0,1.2*max(rc)),names.arg=as.character(1:c),cex.names=0.6);box()
rm(c,o,v,rc,roc)
}else{tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',
caption="Input Error")}
