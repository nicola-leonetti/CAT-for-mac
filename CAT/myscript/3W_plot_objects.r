# Purpose   : Plot of Objects Loadings from a PCA3W object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
if(exists("PCA3W",envir=.GlobalEnv)){
obj<-inpboxe('Variable containing the labels (e.g., A[,1])','None','Objects')
  if(!is.null(obj)){
  lo<-t(PCA3W$lo)
  siz=.9-log10(dim(lo)[1])/10 # defines the size of the characters in the plots, based on the number of samples
  ldim<-c(min(lo,0),max(lo,0))
  dev.new(title="loading plot objects")
  op<-par(pty='s')
  plot(lo[,1],lo[,2],type='n',main='Plot of Objects',xlim=ldim,ylim=ldim,
       xlab='Axis 1',ylab='Axis 2');grid()
  if(obj=='None')text(lo[,1],lo[,2],as.character(1:PCA3W$limits[2]),col='red',cex=siz)
  points(0,0,pch='+',col='black')
  if(obj!='None'){
    text(lo[,1],lo[,2],makevar(obj[[1]])$value[1:nrow(lo)],col='red',cex=siz)
    points(0,0,pch='+',col='black')
  }
  rm(lo,ldim,obj,op,siz)
}
}else{tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',
caption="Input Error")}


