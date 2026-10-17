# Purpose   : evaluate the residuals in Cross-Validation from a PLS object
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
if(exists("PLS",envir=.GlobalEnv)){
  if(!exists('pls.res'))pls.res<-c('None','None','FALSE')
  ans<-inpboxeek(c('Label Vector (e.g., A[,1])','Color Vector (e.g., A[,1])'),'Row Names',pls.res)
  if(!is.null(ans)){
    pls.res <- ans
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
      tex<-variable$value}
    if(as.logical(ans[[3]]))tex<-row.names(PLS$dataset)
    ans1<-1
    if(PLS$nY>1)ans1<-inpboxc('Which Y variable :',as.character(1:PLS$nY))
    if(!is.null(ans1)){
      ms<-as.matrix(PLS$dataset[,1])[,as.numeric(ans1)]
      dev.new(title="residuals in fitting and CV")
      rs<-PLS$resf$fitted.values[,as.numeric(ans1),PLS$ncomp]-ms
      rs.v<-PLS$res$validation$pred[,as.numeric(ans1),PLS$ncomp]-ms
      if((is.null(g))&(is.null(tex))){
        layout(mat=matrix(c(1,2),nrow=1), widths = c(1,1))
        op<-par(pty='s')
        plot(1:length(rs),rs,type='n',xlab='Object Number',ylim=c(min(0,rs),max(0,rs)),
             ylab=paste('Residuals in Fitting with ',PLS$ncomp,' Comp.'));grid();
        abline(h=0,col="red")
        points(1:length(rs),rs,col='black')
        plot(1:length(rs.v),rs,xlab='Object Number',ylab=paste('Residuals in CV with ',PLS$ncomp,
                                                             ' Comp.'),type='n',ylim=c(min(0,rs.v),max(0,rs.v)));grid()
        abline(h=0,col="red")
        points(1:length(rs.v),rs.v,col='black')
      }
      if((!is.null(g))&(is.null(tex))){
        if(tog=="factor"|tog=="character"){
          layout(mat=matrix(c(1,2),nrow=1), widths = c(1,1))
          op<-par(pty='s')
          plot(1:length(rs),rs,type='n',xlab='Object Number',ylim=c(min(0,rs),max(0,rs)),
               ylab=paste('Residuals in Fitting with ',PLS$ncomp,' Comp.'));grid();
          abline(h=0,col="red")
          points(1:length(rs),rs,col=vcolor[as.numeric(g)],pch=16)
          legend("top",inset=c(0,-0.15), legend=lev,col=vcolor, cex=0.8,pch=18,ncol=min(length(lev),4),xpd=TRUE,bty = "n")
          plot(1:length(rs.v),rs,xlab='Object Number',ylab=paste('Residuals in CV with ',PLS$ncomp,
                                                                 ' Comp.'),type='n',ylim=c(min(0,rs.v),max(0,rs.v)));grid()
          abline(h=0,col="red")
          points(1:length(rs),rs,col=vcolor[as.numeric(g)],pch=16)
          legend("top",inset=c(0,-0.15), legend=lev,col=vcolor, cex=0.8,pch=18,ncol=min(length(lev),4),xpd=TRUE,bty = "n")
        }else{
          layout(mat=matrix(c(1,2,3),nrow=1),widths = c(1,1,0.2),respect=TRUE)
          op<-par(pty='s')
          par(mar = c(4, 5, 4, 0))
          plot(1:length(rs),rs,type='n',xlab='Object Number',ylim=c(min(0,rs),max(0,rs)),cex.lab=1.5, cex.main=1.8,
               ylab=paste('Residuals in Fitting with ',PLS$ncomp,' Comp.'));grid();
          abline(h=0,col="red")
          points(1:length(rs),rs,col=vcolor[as.numeric(g)],pch=16)
          par(mar = c(4, 1, 4, 0))
          plot(1:length(rs.v),rs,xlab='Object Number',ylab=paste('Residuals in CV with ',PLS$ncomp,' Comp.'),cex.lab=1.5, cex.main=1.8,
                                                                 type='n',ylim=c(min(0,rs.v),max(0,rs.v)));grid()
          abline(h=0,col="red")
          points(1:length(rs),rs,col=vcolor[as.numeric(g)],pch=16)
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
        plot(1:length(rs),rs,type='n',xlab='Object Number',ylim=c(min(0,rs),max(0,rs)),
             ylab=paste('Residuals in Fitting with ',PLS$ncomp,' Comp.'));grid();
        abline(h=0,col="red")
        text(1:length(rs),rs,as.character(tex),cex=0.8,pch=16)
        plot(1:length(rs.v),rs,xlab='Object Number',ylab=paste('Residuals in CV with ',PLS$ncomp,
                                                               ' Comp.'),type='n',ylim=c(min(0,rs.v),max(0,rs.v)));grid()
        abline(h=0,col="red")
        text(1:length(rs.v),rs.v,as.character(tex),cex=0.8,pch=16)
        }
      if((!is.null(g))&(!is.null(tex))){
        if(tog=="factor"|tog=="character"){
          layout(mat=matrix(c(1,2),nrow=1), widths = c(1,1))
          op<-par(pty='s')
          plot(1:length(rs),rs,type='n',xlab='Object Number',ylim=c(min(0,rs),max(0,rs)),
               ylab=paste('Residuals in Fitting with ',PLS$ncomp,' Comp.'));grid();
          abline(h=0,col="red")
          text(1:length(rs),rs,as.character(tex),col=vcolor[as.numeric(g)],cex=0.8)
          legend("top",inset=c(0,-0.15), legend=lev,col=vcolor, cex=0.8,pch=18,ncol=min(length(lev),4),xpd=TRUE,bty = "n")
          plot(1:length(rs.v),rs,xlab='Object Number',ylab=paste('Residuals in CV with ',PLS$ncomp,
                                                                 ' Comp.'),type='n',ylim=c(min(0,rs.v),max(0,rs.v)));grid()
          abline(h=0,col="red")
          text(1:length(rs.v),rs.v,as.character(tex),col=vcolor[as.numeric(g)],cex=0.8)
          legend("top",inset=c(0,-0.15), legend=lev,col=vcolor, cex=0.8,pch=18,ncol=min(length(lev),4),xpd=TRUE,bty = "n")
        }else{
          layout(mat=matrix(c(1,2,3),nrow=1),widths = c(1,1,0.2),respect=TRUE)
          op<-par(pty='s')
          par(mar = c(4, 5, 4, 0))
          plot(1:length(rs),rs,type='n',xlab='Object Number',ylim=c(min(0,rs),max(0,rs)),cex.lab=1.5, cex.main=1.8,
               ylab=paste('Residuals in Fitting with ',PLS$ncomp,' Comp.'));grid();
          abline(h=0,col="red")
          text(1:length(rs),rs,as.character(tex),col=vcolor[as.numeric(g)],cex=1)
          par(mar = c(4, 1, 4, 0))
          plot(1:length(rs.v),rs,xlab='Object Number',ylab=paste('Residuals in CV with ',PLS$ncomp, ' Comp.'),cex.lab=1.5, cex.main=1.8,
                                                                type='n',ylim=c(min(0,rs.v),max(0,rs.v)));grid()
          abline(h=0,col="red")
          text(1:length(rs.v),rs.v,as.character(tex),col=vcolor[as.numeric(g)],cex=1)
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
      rm(g,tex,rs,rs.v,ans,ans1,vcolor,op,ms)}}}else{
        tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCR/PLS1/PLS2!',
                      caption="Input Error")}
    
    
    
 
