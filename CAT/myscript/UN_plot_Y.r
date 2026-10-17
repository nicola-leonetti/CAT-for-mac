# Purpose   : univariate plot vs. vector position
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 6.6
# Licence   : GPL2.1
#

require(gplots)
if(!exists('Yplot.set'))Yplot.set<-c(previous.name,'None','None','FALSE','TRUE')
ans<-inpboxeeekk(c('*Matrix Name (e.g., A[,1])','Label Vector (e.g., A[,1])','Color Vector (e.g., A[,1])'),
c('Line','Points'),Yplot.set)
if(!is.null(ans)){
previous.name<-ans[[1]]
Yplot.set<-ans
if(!grepl('[', ans[[1]], fixed = TRUE)){
  yvar<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  xymatrix<-data.frame(x=1:nrow(yvar),y=yvar,f=rep(0,length(yvar)),g=unlist(dovc(rep(0,length(yvar)))))
  col_name<-'Variable value'
}else{
  yvar<-eval(parse(text=extraction(ans[[1]])$obj_name),envir=.GlobalEnv)[extraction(ans[[1]])$rows,extraction(ans[[1]])$columns]
  xymatrix<-data.frame(x=extraction(ans[[1]])$rows,y=yvar,f=rep(0,length(yvar)),g=unlist(dovc(rep(0,length(yvar)))))
  col_name <- extraction(ans[[1]])$col_names
}
if(ans[[2]]!='None')xymatrix$f<-as.character(eval(parse(text=ans[[2]]),envir=.GlobalEnv))
if(ans[[3]]!='None'){
	variable<-eval(parse(text=ans[[3]]),envir=.GlobalEnv)
	if(is.factor(variable))variable<-as.character(variable)
	xymatrix$g<-unlist(dovc(variable))}
if(sum(is.na(xymatrix))!=0)print('>>NA found and ignored<<')
xymatrix<-na.omit(xymatrix)
dev.new(title=paste("Y plot",col_name))
plot(c(min(xymatrix[,1]),max(xymatrix[,1])),c(min(xymatrix[,2]),max(xymatrix[,2])),'n',xlab='Object Index',
     ylab=col_name)
grid()
if(as.logical(ans[[4]]))lines(xymatrix[,1],xymatrix[,2],col='red')
if(as.logical(ans[[5]])&(ans[[2]]=='None'))points(xymatrix[,1],xymatrix[,2],col=as.character(xymatrix[,4]),pch=16)
if(ans[[2]]!='None')text(xymatrix[,1],xymatrix[,2],labels=xymatrix[,3],col=as.character(xymatrix[,4]))
if(ans[[3]]!='None'){
  if(is.character(variable))legend('top',legend=unique(variable),col=unique(xymatrix$g),horiz=TRUE,pch=16)
}
rm(yvar,xymatrix,col_name)
}