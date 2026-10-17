# Purpose   : Plot of Variables Loadings from a PCA3W object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
if(exists("PCA3W",envir=.GlobalEnv)){
  ans<-inpboxek('Variable labels (e.g., a,b)','Column header',vinp=c('None','TRUE'),title='Variables')
  if (!is.null(ans)){
    lv<-t(PCA3W$lv)
    siz=.9-log10(dim(lv)[1])/10 # defines the size of the characters in the plots, based on the number of samples
    dev.new(title="loading plot variables")
    op<-par(pty='s')
    ldim<-c(min(lv,0),max(lv,0))
    plot(lv[,1],lv[,2],type='n',main='Plot of Variables',xlim=ldim,ylim=ldim,
         xlab='Axis 1',ylab='Axis 2');grid()
    txt_<-as.character(1:PCA3W$limits[3])
    if(ans[[2]])txt_<-attr(lv, "dimnames")[[1]]
    if(ans[[1]]!='None')txt_<-unlist(strsplit(ans[[1]],","))
    text(lv[,1],lv[,2],txt_,col='green',cex=siz)
    points(0,0,pch='+',col='black')
    rm(lv,ldim,txt_,op,siz)
  }
}else{tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',
caption="Input Error")}
