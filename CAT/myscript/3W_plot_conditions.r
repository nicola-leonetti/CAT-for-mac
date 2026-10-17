# Purpose   : Plot of Conditions Loadings from a PCA3W object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
if(exists("PCA3W",envir=.GlobalEnv)){
obj<-inpboxe('Variable containing the labels (e.g., A[,2])','None','Conditions')
if(!is.null(obj)){
  lc<-t(PCA3W$lc)
  siz=.9-log10(dim(lc)[1])/10 # defines the size of the characters in the plots, based on the number of samples
  dev.new(title="loading plot conditions")
  op<-par(pty='s')
  ldim<-c(min(lc,0),max(lc,0))
  plot(lc[,1],lc[,2],type='n',main='Plot of Conditions',xlim=ldim,ylim=ldim,
       xlab='Axis 1',ylab='Axis 2');grid()
  if(obj=='None')text(lc[,1],lc[,2],as.character(1:PCA3W$limits[1]),col='blue',cex=siz)
  points(0,0,pch='+',col='black')
  if(obj!='None'){
    text(lc[,1],lc[,2],unique(makevar(obj[[1]])$value),col='blue',cex=siz)
    points(0,0,pch='+',col='black')
  }
  rm(lc,ldim,obj,op,siz)
}
}else{tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',
caption="Input Error")}
