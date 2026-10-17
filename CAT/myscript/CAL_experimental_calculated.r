# Purpose   : evaluate the fitted value in Cross-Validation from a PLS object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 2.1
# Licence   : GPL2.1
#
if(exists("PLS",envir=.GlobalEnv)){
  if(!exists('pls.ex_cal'))pls.ex_cal<-c('None','None','FALSE')
ans<-inpboxeek(c('Label Vector (e.g., A[,1])','Color Vector (e.g., A[,1])'),'Row Names',pls.ex_cal)
if(!is.null(ans)){
 pls.ex_cal = ans
 require(gplots)
 g<-NULL;tex<-NULL;vcolor<-NULL
 if(ans[[2]]!='None'){
   variable<-makevar(ans[[2]])
   if(variable$control==1)tl=variable$input
   if(variable$control==2)tl=colnames(eval(parse(text=variable$name),envir=.GlobalEnv))[as.numeric(sub("]","",sub(".*\\[,", "", sub("[[:digit:]]+","",sub(":","",sub("[[:digit:]]+","", variable$surname))))))]
   if(variable$control==3)tl=colnames(eval(parse(text=variable$name),envir=.GlobalEnv))[as.numeric(sub("]","",sub(".*\\[,", "", variable$surname)))]
   g<-variable$value
   if(!is.null(g)){
     tog<-typeof(g)
     if(is.factor(g))tog<-"factor"
     g<-factor(g)
     lev<-levels(g)
     nl<-nlevels(g)
     if(tog=="double")vcolor<-unlist(dovc(as.numeric(lev)))
     if(tog=="factor")vcolor<-unlist(dovc(as.character(lev)))
     if(tog=="character")vcolor<-unlist(dovc(as.character(lev)))
     if(tog=="integer")vcolor<-unlist(dovc(as.numeric(lev)))
     }
 }
 if(ans[[1]]!='None'){
 variable<-makevar(ans[[1]])
 tex<-variable$value
 }
 if(as.logical(ans[[3]]))tex<-row.names(PLS$dataset)
 ans1<-1
 if(PLS$nY>1)ans1<-inpboxc('Which Y variable :',as.character(1:PLS$nY))
 if(!is.null(ans1)){
 ms<-as.matrix(PLS$dataset[,1])[,as.numeric(ans1)]
 dev.new(title="experimental vs fitted and CV predicted")
 ft<-PLS$resf$fitted.values[,as.numeric(ans1),PLS$ncomp]
 ft.v<-PLS$res$validation$pred[,as.numeric(ans1),PLS$ncomp]
 yl<-c(min(ft,ft.v,ms),max(ft,ft.v,ms))
 yl.v<-c(min(ft,ft.v,ms),max(ft,ft.v,ms))
 if((is.null(g))&(is.null(tex))){
   layout(mat=matrix(c(1,2),nrow=1), widths = c(1,1))
   op<-par(pty='s')
   plot(ms,ft,xlab='Experimental Value',ylab='Fitted Value',xlim=yl,ylim=yl,
        main=paste('Model with',PLS$ncomp,'Comp.'),type='n')
   lines(par('usr')[1:2],par('usr')[3:4]);grid()
   points(ms,ft,col='black')
   plot(ms,ft.v,xlab='Experimental Value',ylab='CV Value',xlim=yl.v,ylim=yl.v,
        main=paste('Model with',PLS$ncomp,'Comp.'),type='n')
   lines(par('usr')[1:2],par('usr')[3:4]);grid()
   points(ms,ft.v,col='black')
 }
 if((!is.null(g))&(is.null(tex))){
   if(tog=="factor"|tog=="character"){
     layout(mat=matrix(c(1,2),nrow=1), widths = c(1,1))
     op<-par(pty='s')
     plot(ms,ft,xlab='Experimental Value',ylab='Fitted Value',xlim=yl,ylim=yl,
          main=paste('Model with',PLS$ncomp,'Comp.'),type='n')
     lines(par('usr')[1:2],par('usr')[3:4]);grid()
     points(ms,ft,col=vcolor[as.numeric(g)])
     legend("topleft", legend=lev,col=vcolor, cex=0.8,pch=18,ncol=min(length(lev),4),xpd=TRUE,bty = "n")
     plot(ms,ft.v,xlab='Experimental Value',ylab='CV Value',xlim=yl.v,ylim=yl.v,
          main=paste('Model with',PLS$ncomp,'Comp.'),type='n')
     lines(par('usr')[1:2],par('usr')[3:4]);grid()
     points(ms,ft.v,col=vcolor[as.numeric(g)])
     legend("topleft", legend=lev,col=vcolor, cex=0.8,pch=18,ncol=min(length(lev),4),xpd=TRUE,bty = "n")
   }else{
     layout(mat=matrix(c(1,2,3),nrow=1),widths = c(1,1,0.2),respect=TRUE)
     op<-par(pty='s')
     par(mar = c(4, 5, 4, 0))
     plot(ms,ft,xlab='Experimental Value',ylab='Fitted Value',xlim=yl,ylim=yl,
          main=paste('Model with',PLS$ncomp,'Comp.'),type='n',cex.lab=1.5, cex.main=1.8)
     lines(par('usr')[1:2],par('usr')[3:4]);grid()
     points(ms,ft,col=vcolor[as.numeric(g)])
     par(mar = c(4, 1, 4, 0))
     plot(ms,ft.v,xlab='Experimental Value',ylab='CV Value',xlim=yl.v,ylim=yl.v,
          main=paste('Model with',PLS$ncomp,'Comp.'),type='n',cex.lab=1.5, cex.main=1.8)
     lines(par('usr')[1:2],par('usr')[3:4]);grid()
     points(ms,ft.v,col=vcolor[as.numeric(g)])
     par(op)
     par(mar = c(4, 0.3, 4, 2.7))
     m<-as.numeric(lev)[order(as.numeric(lev),decreasing = FALSE)]
     image(y=m,z=t(m), col=vcolor,
           axes=FALSE, cex.main=.8,ylab='')
     mtext(tl,line=1)
     axis(4,cex.axis=0.8,mgp=c(0,0.5,0))
     rm(tl,m)
   }
 }
 if((is.null(g))&(!is.null(tex))){
   layout(mat=matrix(c(1,2),nrow=1), widths = c(1,1))
   op<-par(pty='s')
   plot(ms,ft,xlab='Experimental Value',ylab='Fitted Value',xlim=yl,ylim=yl,
        main=paste('Model with',PLS$ncomp,'Comp.'),type='n')
   lines(par('usr')[1:2],par('usr')[3:4]);grid()
   text(ms,ft,as.character(tex),cex=0.7)
   plot(ms,ft.v,xlab='Experimental Value',ylab='CV Value',xlim=yl.v,ylim=yl.v,
        main=paste('Model with',PLS$ncomp,'Comp.'),type='n')
   lines(par('usr')[1:2],par('usr')[3:4]);grid()
   text(ms,ft.v,as.character(tex),cex=0.7)
 }
 if((!is.null(g))&(!is.null(tex))){
   if(tog=="factor"|tog=="character"){
     layout(mat=matrix(c(1,2),nrow=1), widths = c(1,1))
     op<-par(pty='s')
     plot(ms,ft,xlab='Experimental Value',ylab='Fitted Value',xlim=yl,ylim=yl,
          main=paste('Model with',PLS$ncomp,'Comp.'),type='n')
     lines(par('usr')[1:2],par('usr')[3:4]);grid()
     text(ms,ft,as.character(tex),col=vcolor[as.numeric(g)],cex=0.7)
     legend("topleft", legend=lev,col=vcolor, cex=0.8,pch=18,ncol=min(length(lev),4),xpd=TRUE,bty = "n")
     plot(ms,ft.v,xlab='Experimental Value',ylab='CV Value',xlim=yl.v,ylim=yl.v,
          main=paste('Model with',PLS$ncomp,'Comp.'),type='n')
     lines(par('usr')[1:2],par('usr')[3:4]);grid()
     text(ms,ft.v,as.character(tex),col=vcolor[as.numeric(g)],cex=0.7)
     legend("topleft", legend=lev,col=vcolor, cex=0.8,pch=18,ncol=min(length(lev),4),xpd=TRUE,bty = "n")
   }else{
     layout(mat=matrix(c(1,2,3),nrow=1),widths = c(1,1,0.2),respect=TRUE)
     op<-par(pty='s')
     par(mar = c(4, 5, 4, 0))
     plot(ms,ft,xlab='Experimental Value',ylab='Fitted Value',xlim=yl,ylim=yl,
          main=paste('Model with',PLS$ncomp,'Comp.'),type='n',cex.lab=1.5, cex.main=1.8)
     lines(par('usr')[1:2],par('usr')[3:4]);grid()
     text(ms,ft,as.character(tex),col=vcolor[as.numeric(g)],cex=1)
     par(mar = c(4, 1, 4, 0))
     plot(ms,ft.v,xlab='Experimental Value',ylab='CV Value',xlim=yl.v,ylim=yl.v,
          main=paste('Model with',PLS$ncomp,'Comp.'),type='n',cex.lab=1.5, cex.main=1.8)
     lines(par('usr')[1:2],par('usr')[3:4]);grid()
     text(ms,ft.v,as.character(tex),col=vcolor[as.numeric(g)],cex=1)
     par(op)
     par(mar = c(4, 0.3, 4, 2.7))
     m<-as.numeric(lev)[order(as.numeric(lev),decreasing = FALSE)]
     image(y=m,z=t(m), col=vcolor,
           axes=FALSE, cex.main=.8,ylab='')
     mtext(tl,line=1)
     axis(4,cex.axis=0.8,mgp=c(0,0.5,0))
     rm(tl,m)
   }
    }
 par(op)
 if(!is.null(g))rm(tog,lev,nl)
 rm(g,tex,ft,ft.v,ms,ans,vcolor,yl,yl.v,op,ans1)}}}else{
tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCR/PLS1/PLS2!'
,caption="Input Error")}
