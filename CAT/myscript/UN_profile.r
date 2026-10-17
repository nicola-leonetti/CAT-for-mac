# Authors   : R. Leardi, C. Melzi, G. Polotti
if(!exists('prof.set'))prof.set<-c(previous.name,'all','all','None')
ans<-inpboxeeeek(c('*Matrix Name','*Rows (e.g., "1:10,15" or "15:end")',
'*Columns (e.g., "1:3,7" or "3:end")','Color Vector (e.g., A[,1])'),'Header on the x axis?',prof.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  prof.set <- ans
  M_<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)

.a <- strsplit(unlist(strsplit(ans[[2]], ',')),':')
if((ans[[2]]!='all')& .a[[length(.a)]][length(.a[[length(.a)]])]=='end'){
  .a[[length(.a)]][2]=nrow(M_)
  for(i in 1:length(.a)).a[[i]]<-paste(.a[[i]],collapse=':')
  ans[[2]]<-paste(unlist(.a),collapse=',')
}
.a <- strsplit(unlist(strsplit(ans[[3]], ',')),':')
if((ans[[3]]!='all')& .a[[length(.a)]][length(.a[[length(.a)]])]=='end'){
  .a[[length(.a)]][2]=ncol(M_)
  for(i in 1:length(.a)).a[[i]]<-paste(.a[[i]],collapse=':')
  ans[[3]]<-paste(unlist(.a),collapse=',')
}
rm(.a)

  if((ans[[2]]!='all')&(ans[[3]]!='all'))M_<-M_[givedim(ans[[2]]),givedim(ans[[3]])]
  if((ans[[2]]!='all')&(ans[[3]]=='all'))M_<-M_[givedim(ans[[2]]),]
  if((ans[[2]]=='all')&(ans[[3]]!='all'))M_<-M_[,givedim(ans[[3]])]
  at <- 1:length((colnames(M_)))
  tk <- TRUE
  # if(length((colnames(M_))>15))tk <- FALSE
  # if(length((colnames(M_)))>9000000000)at=c(seq(1,length(colnames(M_)),(length(colnames(M_))-1)/10),length(colnames(M_)))
  if(length(colnames(M_))>15){
    n<-length((colnames(M_)))
    m<-floor((n-1)/11)
    at=seq(1,n,m)
    rm(n,m)
  }
  assex <- 1:length(colnames(M_))
  if(ans[[5]]){
    assex <- colnames(M_)
  }
  if(is.numeric(t(M_))){
    if(as.character(ans[[4]])=='None'){
      dev.new(title="row profile")
      matplot(y = t(M_),type = "l",lty=1,xlab="",ylab="",xaxt='n')
      axis(side=1, at=at,labels=assex[at],cex.axis=0.8,tick=tk)
    }else{
      variable<-makevar(ans[[4]])
      grade<-variable$value 
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
        if(tog=="character" | tog=="factor"){
          dev.new(title="row profile")
          matplot(y = t(M_),type = "l",lty=1,xlab="",ylab="",col=vcolor[grade],xaxt='n')
          axis(side=1, at=at,labels=assex[at],cex.axis=0.8,tick=tk)
          legend("top", legend=lev,col=vcolor, cex=0.8,lty=1,ncol=min(length(lev),4),inset=c(0,-0.10),xpd=TRUE,bty = "n") 
        }else{
          dev.new(title="row profile")
          layout(mat=matrix(c(1,2),nrow=1),widths = c(5,0.9))
          par(mar = c(4, 3, 4, 0))
          matplot(y = t(M_),type = "l",lty=1,xlab="",ylab="",col=vcolor[grade],xaxt='n')
          axis(side=1, at=at, labels=assex[at],cex.axis=0.8,tick=tk)
          par(mar = c(2, 2, 4, 2))
          m<-as.numeric(lev)[order(as.numeric(lev),decreasing = FALSE)]
          if(variable$control==1)tl=variable$input
          if(variable$control==2)tl=colnames(eval(parse(text=variable$name),envir=.GlobalEnv))[as.numeric(sub("]","",sub(".*\\[,", "", sub("[[:digit:]]+","",sub(":","",sub("[[:digit:]]+","", variable$surname))))))]
          if(variable$control==3)tl=colnames(eval(parse(text=variable$name),envir=.GlobalEnv))[as.numeric(sub("]","",sub(".*\\[,", "", variable$surname)))]
          color_matrix <- matrix(1:100, nrow = 1)
          image(
            color_matrix,         
            col = unlist(dovc(color_matrix)),  
            axes = FALSE,
            main=tl, cex.main=.8,ylab=''
          )
          pretty_labels <- function(x, max_dec = 4) {
            dec <- sapply(x, function(v) {
              if (v == 0) return(0)
              nchar(sub("^[^.]*\\.?","", format(v, scientific = FALSE)))
            })
            d <- min(max(dec), max_dec)
            formatC(x, format = "f", digits = d)}
          labs <- pretty(range(variable$value), n = 6)
          labs <- labs[ labs >= min(variable$value) & labs <= max(variable$value) ]
          labs_pos <- (labs - min(variable$value)) / (max(variable$value) - min(variable$value))
         axis(4,cex.axis=0.8,mgp=c(0,0.5,0),
              las=1,
              at = labs_pos,
              labels = pretty_labels(labs, max_dec = 4))
          rm(tl,m,labs,labs_pos,pretty_labels,color_matrix)
        }
        rm(grade,lev,nl,tog,vcolor,tk)
      }
    }
  }else{
    print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
  }
  rm(ans,M_,at,assex)
}

if(exists("i"))rm(i)