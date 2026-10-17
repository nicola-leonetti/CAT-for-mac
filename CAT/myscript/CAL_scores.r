# Purpose   : draw a Y-score plot vs.the component number from a PLS object 
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 2.0
# Licence   : GPL2.1
#
if(exists("PLS",envir=.GlobalEnv)){
ans<-inpboxeeeerr(c('* Component on x-axis', '* Component on y-axis','Label Vector (e.g., A[,1])','Color Vector (e.g., A[,1])'),
c('Same scale','Different scales'),c('1','2','None','None','TRUE','FALSE'))
if(!is.null(ans)){
require(gplots)
dev.new(title="score plot")
n1<-as.integer(ans[[1]])
n2<-as.integer(ans[[2]])
if((n1<=PLS$ncomp)&(n2<=PLS$ncomp)){
Ms<-PLS$res$scores
if(as.logical(ans[[5]])){
    yl<-c(min(Ms[,n1],Ms[,n2]),max(Ms[,n1],Ms[,n2]))
	xl<-yl
}else{ 
    yl<-c(min(Ms[,n2]),max(Ms[,n2]))
	xl<-c(min(Ms[,n1]),max(Ms[,n1]))
}
tex<-NULL;grade<-NULL
if(as.character(ans[[3]])!='None')tex<-eval(parse(text=ans[[3]]),envir=.GlobalEnv)
if(as.logical(ans[[5]]))tex<-rownames(PLS$dataset)
if(as.character(ans[[4]])!='None'){
variable<-makevar(ans[[4]])
grade<-variable$value}
if(!is.null(grade)){
    tog<-typeof(grade)
	if(is.factor(grade))tog<-"factor"
	grade<-factor(grade)
	lev<-levels(grade)
	nl<-nlevels(grade)
	if(tog=="double")vcolor<-unlist(dovc(as.numeric(lev)))
	if(tog=="factor")vcolor<-unlist(dovc(as.character(lev)))
	if(tog=="character")vcolor<-unlist(dovc(as.character(lev)))
	if(tog=="integer")vcolor<-unlist(dovc(as.numeric(lev)))
}
if(is.null(tex) & is.null(grade)){
   plot(Ms[,n1],Ms[,n2],xlab=paste('Component ',n1,sep=''),ylab=paste('Component ',n2,sep=''),
   xlim=xl,ylim=yl,pty='o',col='black');grid()
}
if(!is.null(tex)& is.null(grade)){
   plot(Ms[,n1],Ms[,n2],type='n',xlab=paste('Component ',n1,sep=''),ylab=paste('Component ',n2,sep=''),
   xlim=xl,ylim=yl);grid()
   text(Ms[,n1],Ms[,n2],as.character(tex),col='black',cex=0.8)
}
if(is.null(tex)&!is.null(grade)){
   plot(Ms[,n1],Ms[,n2],type='n',xlab=paste('Component ',n1,sep=''),ylab=paste('Component ',n2,sep=''),
   xlim=xl,ylim=yl);grid()
   for(i in 1:nl){
      points(subset(Ms[,c(n1,n2)],grade==lev[i]),pch=19,col=vcolor[i])
   }
   rm(lev,nl,vcolor)
}
if(!is.null(tex)& !is.null(grade)){
   plot(Ms[,n1],Ms[,n2],type='n',xlab=paste('Component ',n1,sep=''),ylab=paste('Component ',n2,sep=''),
   xlim=xl,ylim=yl);grid()
   for(i in 1:nl){
      text(subset(Ms[,c(n1,n2)],grade==lev[i]),as.character(subset(tex,grade==lev[i])),
	  col=vcolor[i],cex=0.8)
   }
}
text(0,0,'+',cex=1.2,col='red')
rm(ans,n1,n2,Ms,xl,yl,tex,grade)
}else{
tk_messageBox(type=c("ok"),message='Component Number too high!',caption="Input Error")}
}
}else{tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCR/PLS1/PLS2!',
caption="Input Error")}
