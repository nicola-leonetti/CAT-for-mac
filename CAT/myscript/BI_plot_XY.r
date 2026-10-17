# Purpose   : make a X vs. Y scatter plot requiring the name of the vectors,
#             the factor and the grade through a Input box
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.7
# Licence   : GPL2.1
#
library(gplots)
if(!exists('XYplot.set'))XYplot.set<-c('','','None','None','FALSE','TRUE','FALSE')
ans<-inpboxeeeekkk(c('*Variable on x-axis (e.g., A[,1])','*Variable on y-axis (e.g., A[,2])','Label Vector (e.g., B[,1])','Color Vector (e.g., B[,2])'),
c('Line','Points','Row Names'),XYplot.set)
if(!is.null(ans)){
  XYplot.set<-ans
if(ans[[1]]!=''){
	variable<-makevar(ans[[1]])
	x_<-as.vector(unlist(variable$value))
col_name1<-colnames(variable$input)
if (is.null(col_name1)){
  if(grepl('[', ans[[1]], fixed = TRUE))col_name1<-colnames(get(sub("\\[.*", "", ans[[1]])))[as.numeric(sub("]","",sub(".*\\[,", "", ans[[1]])))]
  if(!grepl('[', ans[[1]], fixed = TRUE))col_name1<-'Variable 1'
}}
if(ans[[2]]!=''){
	variable<-makevar(ans[[2]])
	y_<-as.vector(unlist(variable$value))
col_name2<-colnames(variable$input)
if (is.null(col_name2)){
  if(grepl('[', ans[[2]], fixed = TRUE))col_name2<-colnames(get(sub("\\[.*", "", ans[[2]])))[as.numeric(sub("]","",sub(".*\\[,", "", ans[[2]])))]
  if(!grepl('[', ans[[2]], fixed = TRUE))col_name2<-'Variable 2'
}}
if(length(x_)==length(y_)){
xymatrix<-data.frame(x=x_,y=y_,lb=rep('',length(x_)),g=unlist(dovc(rep(1,length(x_)))))
if(as.logical(ans[[7]]))xymatrix$lb<-row.names(eval(parse(text=paste(substr(ans[[1]],1,str_locate(ans[[1]],",")[1]),"]",sep=""))
                                                    ,envir=.GlobalEnv))
if(ans[[3]]!='None')xymatrix$lb<-as.factor(eval(parse(text=ans[[3]]),envir=.GlobalEnv))
if(ans[[4]]!='None'){
	variable<-eval(parse(text=ans[[4]]),envir=.GlobalEnv)
	if(is.factor(variable) | is.character(variable)){
	  if(is.character(variable))variable <- as.factor(variable)
	  lev<-levels(variable)
	  variable<-as.character(variable)}
	xymatrix$g<-unlist(dovc(variable))
	if(nrow(xymatrix)>1){
	  if(sum(is.na(xymatrix))!=0)print('>>NA found and ignored<<')
	  xymatrix<-na.omit(xymatrix)
	  if(ans[[6]] | ans[[7]]){
	    if(is.character(variable)){
	      vcolor<-unlist(dovc(as.character(lev)))
	      dev.new(title="bivariate scatter plot")
	      plot(xymatrix$x,xymatrix$y,xlim=c(min(xymatrix$x),max(xymatrix$x)),
	           ylim=c(min(xymatrix$y),max(xymatrix$y)),'n',xlab=col_name1, ylab=col_name2)
	      siz=.9-log10(nrow(xymatrix))/10 # defines the size of the characters in the plots, based on the number of samples
	      if(as.logical(ans[[5]]))lines(xymatrix$x,xymatrix$y,col='red')
	      if(as.logical(ans[[6]])& ans[[3]]=='None'&!as.logical(ans[[7]]))points(xymatrix$x,xymatrix$y,col=xymatrix$g,pch=16,cex=siz)
	      if(ans[[3]]!='None' | as.logical(ans[[7]]))text(xymatrix$x,xymatrix$y,labels=xymatrix$lb,col=as.character(xymatrix$g),cex=siz)
	      legend("top", legend=lev,col=unlist(dovc(as.character(lev))), cex=0.8,pch=16,ncol=min(length(lev),4),
	             inset=c(0,-0.10),xpd=TRUE,bty = "n")
	    }else{
	      dev.new(title="bivariate scatter plot")
	      layout(mat=matrix(c(1,2),nrow=1),widths = c(5,0.9))
	      par(mar = c(4, 4, 4, 0))
	      plot(xymatrix$x,xymatrix$y,xlim=c(min(xymatrix$x),max(xymatrix$x)),
	           ylim=c(min(xymatrix$y),max(xymatrix$y)),'n',xlab=col_name1, ylab=col_name2)
	      siz=.9-log10(nrow(xymatrix))/10 # defines the size of the characters in the plots, based on the number of samples
	      if(as.logical(ans[[5]]))lines(xymatrix$x,xymatrix$y,col='red')
	      if(as.logical(ans[[6]])& ans[[3]]=='None'&!as.logical(ans[[7]]))points(xymatrix$x,xymatrix$y,col=xymatrix$g,pch=16,cex=siz)
	      if(ans[[3]]!='None' | as.logical(ans[[7]]))text(xymatrix$x,xymatrix$y,labels=xymatrix$lb,col=as.character(xymatrix$g),cex=siz)
	      # legend("top", legend=lev,col=unlist(dovc(as.character(lev))), cex=0.8,pch=16,ncol=min(length(lev),4),
	      #        inset=c(0,-0.10),xpd=TRUE,bty = "n")
	      par(mar = c(2, 2, 4, 2))
	      variable<-makevar(ans[[4]])
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
	      m<-as.numeric(lev)[order(as.numeric(lev),decreasing = FALSE)]
	      image(y=m,z=t(m), col=vcolor,
	            axes=FALSE, cex.main=.8,ylab='')
	      mtext(tl,line=1)
	      axis(4,cex.axis=0.8,mgp=c(0,0.5,0))
	      rm(tl,m)
	       }
	  }else{
	    dev.new(title="bivariate scatter plot")
	    plot(xymatrix$x,xymatrix$y,xlim=c(min(xymatrix$x),max(xymatrix$x)),
	         ylim=c(min(xymatrix$y),max(xymatrix$y)),'n',xlab=col_name1, ylab=col_name2)
	    siz=.9-log10(nrow(xymatrix))/10 # defines the size of the characters in the plots, based on the number of samples
	    if(as.logical(ans[[5]]))lines(xymatrix$x,xymatrix$y,col='red')
	    if(as.logical(ans[[6]])& ans[[3]]=='None'&!as.logical(ans[[7]]))points(xymatrix$x,xymatrix$y,col=xymatrix$g,pch=16,cex=siz)
	    if(ans[[3]]!='None' | as.logical(ans[[7]]))text(xymatrix$x,xymatrix$y,labels=xymatrix$lb,col=as.character(xymatrix$g),cex=siz)
	  }
	}
}else{
  if(nrow(xymatrix)>1){
    if(sum(is.na(xymatrix))!=0)print('>>NA found and ignored<<')
    xymatrix<-na.omit(xymatrix)
    if(ans[[6]] | ans[[7]]){
      dev.new(title="bivariate scatter plot")
      plot(xymatrix$x,xymatrix$y,xlim=c(min(xymatrix$x),max(xymatrix$x)),
           ylim=c(min(xymatrix$y),max(xymatrix$y)),'n',xlab=col_name1, ylab=col_name2)
      siz=.9-log10(nrow(xymatrix))/10 # defines the size of the characters in the plots, based on the number of samples
      if(as.logical(ans[[5]]))lines(xymatrix$x,xymatrix$y,col='red')
      
      if(as.logical(ans[[6]])& ans[[3]]=='None'&!as.logical(ans[[7]]))points(xymatrix$x,xymatrix$y,
                                                                             # col=xymatrix$g,
                                                                             pch=16,cex=siz)
      if(ans[[3]]!='None' | as.logical(ans[[7]]))text(xymatrix$x,xymatrix$y,labels=xymatrix$lb,
                                                      # col=unlist(dovc(as.character(xymatrix$lb))),
                                                      cex=siz)
      # legend("top", legend=levels(xymatrix$lb),col=unlist(dovc(as.character(levels(xymatrix$lb)))), cex=0.8,pch=16,ncol=min(length(levels(xymatrix$lb)),4),
      #        inset=c(0,-0.10),xpd=TRUE,bty = "n")
    }else{
      dev.new(title="bivariate scatter plot")
      plot(xymatrix$x,xymatrix$y,xlim=c(min(xymatrix$x),max(xymatrix$x)),
           ylim=c(min(xymatrix$y),max(xymatrix$y)),'n',xlab=col_name1, ylab=col_name2)
      siz=.9-log10(nrow(xymatrix))/10 # defines the size of the characters in the plots, based on the number of samples
      if(as.logical(ans[[5]]))lines(xymatrix$x,xymatrix$y,col='red')
      if(as.logical(ans[[6]])& ans[[3]]=='None'&!as.logical(ans[[7]]))points(xymatrix$x,xymatrix$y,col=xymatrix$g,pch=16,cex=siz)
      if(ans[[3]]!='None' | as.logical(ans[[7]]))text(xymatrix$x,xymatrix$y,labels=xymatrix$lb,col=as.character(xymatrix$g),cex=siz)
    }
  }
}
rm(xymatrix,x_,y_,col_name1,col_name2)
}else{
tk_messageBox(type=c('ok'),message='Vectors must have the same length',caption='Input Error')}}

if(exists("ans"))rm(ans)
if(exists("siz"))rm(siz)
if(exists("lev"))rm(lev)
if(exists("vcolor"))rm(vcolor)
if(exists("color_matrix"))rm(color_matrix)
if(exists("g"))rm(g)
if(exists("nl"))rm(nl)
if(exists("tog"))rm(tog)
