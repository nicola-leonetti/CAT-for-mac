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
rn<-row.names(KNN$matrix)
if(is.null(row.names(KNN$matrix)))rn<-c(1:nrow(KNN$matrix))

ans<-inpboxc(tcombo = "row name",vcombo = rn,inp = -1)

if(!is.null(ans)){
n<-which(rn==rn[ans[[1]]])
i=KNN$cv.segm[n]
m<-(n-1)%/%max(KNN$cv.segm)+1
CL<-data.frame(NULL)
for (k in 1:KNN$k.max){
    index <- which(KNN$cv.segm!=i)
    train.cv <- KNN$matrix[index,]
    test.cv <- KNN$matrix[-index,]
    prev<-knn.prev(train = train.cv, test = test.cv, method=KNN$method,cl=KNN$labels[index], k = k)
    CL.k<-cbind.data.frame(k=k,cl=levels(as.factor(prev[[m]])),qt=summary(as.factor(prev[[m]])))
    CL<-rbind(CL,CL.k)
  }
  
require(lattice)
p<-barchart(qt~as.factor(k),groups=cl,CL,auto.key=list(columns =length(levels(CL$cl))),ylim=c(0,max(CL$qt)+0.5),
         ylab="",xlab="k",scales = list(y = list(tck = c(1,0), alternating = 1, at = c(0:KNN$k.max))))

dev.new(title="k neighbours CV")
print(p)
panel.text(30,15,rn[ans[[1]]],cex=1,col='red')

rm(CL,CL.k,i,index,k,k.nearest.neighbors,knn.prev,m,Mahalanobis,n,p,prev,rn,test.cv,train.cv)
}
}else{
  tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")}
