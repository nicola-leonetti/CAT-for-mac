# Purpose   : prediction of test objects
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: CM
# Version   : 2.2
# Licence   : GPL2.1

Mahalanobis<-function (data, cov, inverted = FALSE){
  if (!inherits(data, c("data.frame", "matrix"))) 
    stop("data must be a data.frame or matrix!")
  stopifnot(is.matrix(cov))
  if (ncol(data) != ncol(cov)) 
    stop("incompatible dimensions!")
  x <- as.matrix(data)
  n <- nrow(x)
  D2 <- matrix(0, n, n)
  dimnames(D2) <- list(rownames(data), rownames(data))
  if (!inverted) {
    for (i in 1:n) {
      for (j in 1:n) {
        #if (i > j) 
        D2[i, j] <- crossprod((x[i, ] - x[j, ]), solve(cov, 
                                                       (x[i, ] - x[j, ])))
      }
    }
  }
  else {
    for (i in 1:n) {
      for (j in 1:n) {
        if (i > j) 
          D2[i, j] <- crossprod((x[i, ] - x[j, ]), crossprod(cov, 
                                                             (x[i, ] - x[j, ])))
      }
    }
  }
  return(D2)
}

k.nearest.neighbors <- function(i, distance.matrix, k = k){
  ordered.neighbors <- order(distance.matrix[i, ]) 
  return(ordered.neighbors[1:k])
}

knn.prev<-function(train,test,method,cl,k){
  if (method=="mahalanobis"){
    d<-as.matrix(sqrt(Mahalanobis(rbind(train,test),cov(rbind(train,test)))))
  } else {
    d<-as.matrix(dist(rbind(train,test),method=method)) 
  }
  n.train<-nrow(train)
  n.test<-nrow(test)
  n<-n.train+n.test
  d<-d[(n.train+1):n,1:n.train,drop=FALSE]
  near.cl<-list(NULL)
  prev.cl<-c(NULL)
  n.class<-list(NULL)
  n.class.rnd<-c(NULL)
  for (i in 1:n.test){
    near.cl[[i]]<-summary(cl[k.nearest.neighbors(i,d,k)]) 
    n.class[[i]]<-which(max(near.cl[[i]])==near.cl[[i]])
    if (length(n.class[[i]])==1){
      prev.cl[i]<-levels(cl)[which(max(near.cl[[i]])==near.cl[[i]])]
    } else {
      prev.cl[i]="NA"
    }
  } 
  return(prev.cl) 
}


if(exists("KNN",envir=.GlobalEnv)){
  if(!exists('KNN.pred.set'))KNN.pred.set<-c(previous.name,'all','all','None','')
  ans<-inpboxeeeee(c('*Matrix with samples to be predicted',
                     '*Rows (e.g., "1:10,15" or "15:end")',
                     '*X-Variables (e.g., "1:3,7" or "3:end")',
                     'Category Variable','*k'),KNN.pred.set)
  if(!is.null(ans)){
    previous.name<-ans[[1]]
    KNN.pred.set<-ans
    M_<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
    if(ans[[4]]!='None')labels<-M_[,as.numeric(ans[[4]])]
    
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
    
    if(KNN$centered)M_<-as.matrix(sweep(x = M_, MARGIN = 2, STATS = attr(KNN$matrix,"scaled:center"), FUN = "-"))
    if(KNN$scale) M_<-as.matrix(sweep(x = M_, MARGIN = 2, STATS = attr(KNN$matrix,"scaled:scale"), FUN = "/"))
    prev<-knn.prev(train = KNN$matrix,test = M_,method = KNN$method,
                   cl = KNN$labels,k =ans[[5]])
    Prev<-as.data.frame(prev)
    row.names(Prev)<-rownames(M_)
    names(Prev)=""
    
    if(ans[[4]]=="None"){
      cat(' Category Prediction',"\n")
      print(Prev)}
    
    if(ans[[4]]!="None"){
      prev<-as.factor(prev)
      if("NA" %in% levels(prev)){
        prev <- factor(prev,levels = c(levels(KNN$labels),"NA"))
        # prev<-factor(prev,
        #                 levels(prev)[c(c(1:length(levels(prev)))[-which(levels(prev)=="NA")],which(levels(prev)=="NA"))])
      }else{
        prev <- factor(prev,levels = levels(KNN$labels))
      }
      
      labels <- factor(labels,levels = levels(KNN$labels))
      Perc.cat<-data.frame(NULL)
      cat(' ',"\n")
      cat('Confusion Matrix in Prediction',"\n")
      #print(table(labels,as.factor(prev),dnn = c("Class","Predicted")),envir=.GlobalEnv)
      tab<-table(labels,as.factor(prev),dnn = c("Class","Predicted"));print(tab)
      cat('Labels of non correctly predicted samples',"\n")
      cat(" ",rownames(as.data.frame(M_)[as.character(prev)!=as.character(labels),]),"\n")
      cat('% Correct Predictions',"\n")
      Perc.cat.k<-data.frame(cat=attr(tab,"dimnames")[[1]],k=ans[[5]],perc=rep(0,nrow(tab)))
      for (i in 1:nrow(tab)){
        Perc.cat.k[i,3]=round((tab[i,i]/sum(tab[i,]))*100,digits = 2)
      }
      Perc.cat<-rbind(Perc.cat,Perc.cat.k)
      Perc.cat <- Perc.cat[!is.na(Perc.cat[,3]),]
      
      tab1<-t(Perc.cat[Perc.cat$k==ans[[5]],3])
      #colnames(tab1)<- levels(Perc.cat[Perc.cat$k==ans[[5]],1])
      colnames(tab1)<- unique(Perc.cat$cat)
      rownames(tab1)<-c("")
      tab1<-as.table(tab1);print(tab1)
      cat('% Total Correct Predictions',"\n")
      cat(" ",round(mean(Perc.cat[Perc.cat$k==ans[[5]],3],na.rm=TRUE),digits = 2),"\n")
    }
    
    KNN$matrix_pred<-M_
    KNN$k_pred<-ans[[5]]
    
    rm(M_,prev,Prev)
    rm(knn.prev,k.nearest.neighbors)
    rm(Mahalanobis)
    if(ans[[4]]!='None') rm(tab,labels,Perc.cat,Perc.cat.k,tab1,i)}
}else{
  tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")}

  
  
  
