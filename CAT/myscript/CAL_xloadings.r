# Purpose   : draw a x-loading plot vs.variable number from a PLS object given a 
#             component range 
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.2
# Licence   : GPL2.1
#
require(stringr)
if(exists("PLS",envir=.GlobalEnv)){
ans<-inpboxrr('Plot Type  ',c('Scatter','Lines'))
if(!is.null(ans)){
 require(gplots)
 if(as.logical(ans[[1]])){
   ans1<-inpboxeeekk(c('*Component on x-axis','*Component on y-axis','External Vector with Variable Names (e.g., A[1,])'),
   c('Column Names','Arrows'),c('1','2','None','FALSE','FALSE'))
   if(!is.null(ans1)){
      n1<-as.integer(ans1[[1]])
      n2<-as.integer(ans1[[2]])
      T<-loadings(PLS$res)
      tex<-as.character(1:nrow(T))
      if(ans[[3]]!='None'){
        variable<-makevar(ans[[3]])
        tex<-variable$value
	  }
      if(as.logical(ans[[4]]))tex<-row.names(T)
      Tlim<-c(min(T[,c(n1,n2)]),max(T[,c(n1,n2)]))
      Tlim<-c(sign(Tlim[1])*max(abs(Tlim)),sign(Tlim[2])*max(abs(Tlim)))
      dev.new(title="x loadings scatter plot")
      plot(T[,n1],T[,n2],xlab=paste('Component ',n1),ylab=paste('Component ',n2),
	  main='X-loading Plot',type='n',xlim=Tlim,ylim=Tlim)
      text(T[,n1],T[,n2],tex,cex=0.6)
	  text(0,0,'+',cex=1.2,col='red')
	  grid()
      if(as.logical(ans[[5]]))
	     arrows(rep(0,dim(T)[1]),rep(0,dim(T)[2]),T[,n1],T[,n2],col='red')
      rm(n1,n2,tex,Tlim)
   }
 }else{
      # ans1<-gnfib('*Components to be plotted (e.g.,1,3,5)','1,2')
      ans1<-inpboxek('*Components to be plotted (e.g.,1,3,5)','Header on the x axis?',vinp=c('1,2','TRUE'),title='Input')
      T<-loadings(PLS$res)
      at <- 1:length(rownames(T))
      tk <- TRUE
      # if(length(rownames(T))>15)tk <- FALSE
      if(length(( rownames(T)))>15){
        n<-length(rownames(T))
        m<-floor((n-1)/11)
        at=seq(1,n,m)
        rm(n,m)
      }
      assex <- 1:length(rownames(T))
      x_lab <- 'Variable Number'
      if(ans1[[2]]){
        assex <- PLS$naM_0[-1]
        x_lab <- ''
      }
      dev.new(title="x loadings line plot")
      plot(T[,1],ylab='x-loading value',xlab=x_lab,type='n',ylim=c(min(T),max(T)),xaxt='n')
      axis(side=1, at=at,labels=assex[at],cex.axis=0.8,tick=tk)
      vi<-as.numeric(unlist(str_split(ans1[[1]],',')))
	  grid()
      for(i in vi)lines(T[,i],col=i)
	  legend("bottomleft",legend=as.character(vi),col=vi,lty=1)
	  rm(at,assex,x_lab,tk)
	  rm(i,vi)
 }
 rm(ans1,T)
}
}else{tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCR/PLS1/PLS2!',
caption="Input Error")}
