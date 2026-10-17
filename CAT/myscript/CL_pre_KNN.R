# Authors   : R. Leardi, C. Melzi, G. Polotti
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
    near.cl[[i]]<-as.character(cl[k.nearest.neighbors(i,d,k)])
  }
  return(near.cl) 
}

if(exists("KNN",envir=.GlobalEnv)){
  if(!is.null(KNN$matrix_pred)){
  rn<-row.names(KNN$matrix_pred)
  
  ans<-inpboxc(tcombo = "row name",vcombo = rn,inp = -1)
  
  if(!is.null(ans)){
    n<-which(row.names(KNN$matrix_pred)==rn[ans[[1]]])

    CL<-data.frame(NULL)
    for (k in 1:KNN$k.max){
      train.cv <- KNN$matrix
      test.cv <- KNN$matrix_pred
      prev<-knn.prev(train = train.cv, test = test.cv, method=KNN$method,cl=KNN$labels, k = k)
      CL.k<-cbind.data.frame(k=k,cl=levels(as.factor(prev[[n]])),qt=summary(as.factor(prev[[n]])))
      CL<-rbind(CL,CL.k)
    }
    
    require(lattice)
    p<-barchart(qt~as.factor(k),groups=cl,CL[CL$k==KNN$k_pred,],auto.key=list(columns =length(levels(CL$cl))),ylim=c(0,max(CL$qt)+0.5),
                ylab="",xlab="k",scales = list(y = list(tck = c(1,0), alternating = 1, at = c(0:KNN$k.max))))
    
    dev.new(title="k neighbours prediction")
    print(p)
    panel.text(30,15,rn[ans[[1]]],cex=1,col='red')
    
}}else{
  tk_messageBox(type=c("ok"),message="Run Prediction first!",caption="Input Error")
}}else{
  tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")
}

if(exists("CL"))rm(CL)
if(exists("CL.k"))rm(CL.k)
if(exists("k"))rm(k)
if(exists("k.nearest.neighbors"))rm(k.nearest.neighbors)
if(exists("knn.prev"))rm(knn.prev)
if(exists("Mahalanobis"))rm(Mahalanobis)
if(exists("n"))rm(n)
if(exists("p"))rm(p)
if(exists("prev"))rm(prev)
if(exists("rn"))rm(rn)
if(exists("test.cv"))rm(test.cv)
if(exists("train.cv"))rm(train.cv)