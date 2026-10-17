if(!exists('clog.set'))clog.set<-c(previous.name,'','','')

clog.set<-c(previous.name,'all')
ans<-inpboxee(c('*Matrix Name','*Variables to be selected (e.g., "1:4,8" or "4:end")'),
              clog.set)

ans1<-ans
if(!is.null(ans)){
  previous.name<-ans[[1]]
  clog.set<-ans
  var.trans<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  M <- var.trans
  
  .a <- strsplit(unlist(strsplit(ans[[2]], ',')),':')
  if((ans[[2]]!='all')& .a[[length(.a)]][length(.a[[length(.a)]])]=='end'){
    .a[[length(.a)]][2]=ncol(var.trans)
    for(i in 1:length(.a)).a[[i]]<-paste(.a[[i]],collapse=':')
    ans1[[2]]<-paste(unlist(.a),collapse=',')
  }
  rm(.a)
  
  if(!ans[[2]]=='all')M<-var.trans[,givedim(ans1[[2]]),drop=FALSE]

    for (i in 1:ncol(M)){
      if(min(M[,i],na.rm = TRUE)>0){
        M[,i] <- log10(M[,i])
      }else{
        repeat{
          ans<-gnfib(paste('*Delta value to be added to variable ',colnames(M)[i],' (>', -min(M[,i],na.rm = TRUE),')?',sep='',''))
          wm <- tryCatch(log10(M[,i]+as.numeric(ans[[1]])),
                         warning = function(x) paste('Invalid delta value for',colnames(M)[i]))
          if(is.numeric(wm)&min(wm,na.rm=TRUE)>-Inf){
            M[,i] <- wm
            rm(wm)
            break
          }
        }
      }
    }
    
    if(ans1[[2]]=='all')var.trans<-M
    if(!ans1[[2]]=='all')var.trans[,givedim(ans1[[2]])]<-M
    assign(paste(ans1[[1]],'.log',sep=''),var.trans,envir=.GlobalEnv)
    rm(M,i,var.trans)
    print(paste('The value is saved in: ',ans1[[1]],'.log',sep=''))
    rm(ans1)
}