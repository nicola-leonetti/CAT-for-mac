# Purpose   : TriPlot of Objects/Variables/Conditions Loadings from a PCA3W object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.0
# Licence   : GPL2.1
#
if(exists("PCA3W",envir=.GlobalEnv)){
  
  obj<-inpboxe('Variable containing the labels (e.g., A[,1])','None','Objects')
  if(!is.null(obj)){  
    ans<-inpboxek('Variable labels (e.g., a,b)','Column header',vinp=c('None','TRUE'),title='Variables')
    if (!is.null(ans)){
      cdt<-inpboxe('Variable containing the labels (e.g., A[,2])','None','Conditions')
      if(!is.null(cdt)){
        lo<-t(PCA3W$lo)
        lv<-t(PCA3W$lv)
        lc<-t(PCA3W$lc)
        siz=.9-log10(dim(lo)[1]+dim(lv)[1]+dim(lc)[1])/10 # defines the size of the characters in the plots
        dev.new(title="triplot")
        op<-par(pty='s')
        ldim1<-c(min(lc[,1],lv[,1],lo[,1],0),max(lc[,1],lv[,1],lo[,1],0))
        ldim2<-c(min(lc[,2],lv[,2],lo[,2],0),max(lc[,2],lv[,2],lo[,2],0))
        plot(lo[,1],lo[,2],type='n',main='Triplot (red=objects, green=variables, blue=conditions)',
             cex.main=0.8,xlim=ldim1,ylim=ldim2,xlab='Axis 1',ylab='Axis 2');grid()
        if(obj=='None')text(lo[,1],lo[,2],as.character(1:PCA3W$limits[2]),col='red',cex=siz)
        if(obj!='None')text(lo[,1],lo[,2],makevar(obj[[1]])$value[1:nrow(lo)],col='red',cex=siz)
        txt_<-as.character(1:PCA3W$limits[3])
        if(ans[[2]])txt_<-attr(lv, "dimnames")[[1]]
        if(ans[[1]]!='None')txt_<-unlist(strsplit(ans[[1]],","))
        text(lv[,1],lv[,2],txt_,col='green',cex=siz)
        if(cdt=='None')text(lc[,1],lc[,2],as.character(1:PCA3W$limits[1]),col='blue',cex=siz)
        if(cdt!='None')text(lc[,1],lc[,2],unique(makevar(cdt[[1]])$value),col='blue',cex=siz)
        points(0,0,pch='+',col='black')
      }
    }
  }
}else{tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',
caption="Input Error")}

if(exists('lo'))rm(lo)
if(exists('lv'))rm(lv)
if(exists('lc'))rm(lc)
if(exists('obj'))rm(obj)
if(exists('ans'))rm(ans)
if(exists('cdt'))rm(cdt)
if(exists('ldim1'))rm(ldim1)
if(exists('ldim2'))rm(ldim2)
if(exists('op'))rm(op)
if(exists('siz'))rm(siz)
if(exists('txt_'))rm(txt_)
