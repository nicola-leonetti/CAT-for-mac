# Purpose   : KNN classification
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 2.5
# Licence   : GPL2.1
#
Mahalanobis<-function (data, cov, inverted = FALSE) 
{
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
  d<-d[(n.train+1):n,1:n.train]
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


dist<-c("euclidean","mahalanobis","maximum","manhattan","canberra","binary","minkowski")
if(!exists('KNN.set'))KNN.set<-c(previous.name,"all","","1","7","5",TRUE,TRUE)
ans<-inpboxe6ck2(c('*Matrix Name','*Rows (e.g., "1:10,15" or "15:end")','*X-Variables (e.g., "1:3,7" or "3:end")',
                              '*Category Variable','*k max','*Distance','*Number of segments for CV','Centered','Scaled'),
                 vcombo=dist,
                 as.vector(unlist(KNN.set)))

if(!is.null(ans)){
  previous.name<-ans[[1]]
  KNN.set<-list(ans[[1]],ans[[2]],ans[[3]],ans[[4]],ans[[5]],ans[[7]],ans[[8]],ans[[9]])
  M_<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
        if((ans[[2]]!='all')& strsplit(unlist(strsplit(ans[[2]], ',')),':')[[1]][2]=='end')
         ans[[2]]<-paste(strsplit(unlist(strsplit(ans[[2]], ',')),':')[[1]][1],':',nrow(M_))
        if((ans[[3]]!='all')& strsplit(unlist(strsplit(ans[[3]], ',')),':')[[1]][2]=='end')
         ans[[3]]<-paste(strsplit(unlist(strsplit(ans[[3]], ',')),':')[[1]][1],':',ncol(M_))
	if(ans[[2]]!='all')M_<-M_[givedim(ans[[2]]),]
  rn<-rownames(M_)
  if(ans[[2]]!='all')M_<-M_[givedim(ans[[2]]),]
  labels<-as.factor(M_[,as.numeric(ans[[4]])])
  if(ans[[3]]!='')M_<-M_[,givedim(ans[[3]])]  
  M_<-scale(M_,ans[[8]],ans[[9]])
  k.max<-as.numeric(ans[[5]])
  cv.segm<-rep(c(1:as.numeric(ans[[7]])),nrow(M_)%/%as.numeric(ans[[7]])+1)[1:nrow(M_)]
  DF<-data.frame(NULL)
  
  for(k in 1:k.max){
    DF.k<-data.frame(lab=labels,k=rep(k,nrow(M_)),prev=rep(0,nrow(M_)))
    for(i in 1:as.numeric(ans[[7]])){
      index <- which(cv.segm!=i)
      train.cv <- M_[index,]
      test.cv <- M_[-index,]
      prev<-knn.prev(train = train.cv, test = test.cv, method=dist[ans[[6]]],cl=labels[index], k = k)
      DF.k[-index,3]<-prev
    }
    DF<-rbind(DF,DF.k)
  }
  
  e<-DF$lab==DF$prev
  
  DF$prev<-as.factor(DF$prev)
  
  if("NA" %in% levels(DF$prev)){
    DF$prev<-factor(DF$prev,
                    levels(DF$prev)[c(c(1:length(levels(DF$prev)))[-which(levels(DF$prev)=="NA")],which(levels(DF$prev)=="NA"))])
  }
  
  
  Perc.cat<-data.frame(NULL)
  for (k in 1:k.max){
    cat("    ", "\n")
    cat("k =",k, "\n")
    cat('Confusion Matrix in Cross Validation',"\n")
    tab<-table(DF[DF$k==k,1],DF[DF$k==k,3],dnn = c("Class","Predicted"));print(tab)
    cat('Labels of non correctly assigned Samples',"\n")
    cat(" ",rn[which(!e &  DF$k==k)%%nrow(M_)],"\n")
    #cat(" ",rownames(M_[which(!e &  DF$k==k)%%nrow(M_),,drop=FALSE]),"\n")
    cat('% Correct Predictions in Cross Validation',"\n")
    Perc.cat.k<-data.frame(cat=attr(tab,"dimnames")[[1]],k=k,perc=rep(0,nrow(tab)))
    for (i in 1:nrow(tab)){
      Perc.cat.k[i,3]=round((tab[i,i]/sum(tab[i,]))*100,digits = 2)
    }
    Perc.cat<-rbind(Perc.cat,Perc.cat.k)
    tab1<-t(Perc.cat[Perc.cat$k==k,3])
    colnames(tab1)<- levels(Perc.cat[Perc.cat$k==k,1])
    rownames(tab1)<-c("")
    tab1<-as.table(tab1);print(tab1)
    cat('% Total Correct Predictions in Cross Validation',"\n")
    cat(" ",round(mean(Perc.cat[Perc.cat$k==k,3]),digits = 2),"\n")
  }
  


  
  KNN<-list()
  KNN$matrix<-as.matrix(M_)
  KNN$labels<-labels
  KNN$centered<-ans[[8]]
  KNN$scaled<-ans[[9]]
  KNN$method<-dist[ans[[6]]]
  KNN$k.max<-k.max
  KNN$cv.segm<- cv.segm
  #KNN$k.opt<-DF[order(DF$`% ok`,decreasing = TRUE),1][1]
  KNN$DF<-DF
  
  print('Note: Data are saved in the KNN object, write KNN to see all',quote=FALSE)
  
  
  rm(ans,M_,labels,k.max,DF,DF.k,index,train.cv,test.cv,prev)
  rm(cv.segm,i,k,k.nearest.neighbors,knn.prev,e,Perc.cat,Perc.cat.k)
  rm(dist,tab,tab1)
  rm(Mahalanobis)
  rm(rn)
  }
