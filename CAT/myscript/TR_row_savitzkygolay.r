# Purpose   : perform a Savitzky-Golay transformation
# Input     : 
# Output    : 
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: CM
# Version   : 3.1
# Licence   : GPL2.1

convCppV<-function(u,v){
  m=length(u);n=length(v)
  p=m-n+1
  w<-rep(0,p)
  for(k in 1:p){
    w[k]<-u[k]*v[1]
    for(j in 2:n){
      w[k]<-w[k]+u[k+j-1]*v[j]
    }
  }
  return(w)
}

convCppM<-function(M,v){
  n=nrow(M);m<-ncol(M);f<-length(v)
  p=m-f+1
  X<-matrix(rep(0,n*p),n,p)
  for(i in 1:n){
    u<-as.numeric(M[i,])
    X[i,]<-convCppV(u,v)
  }
  return(X)
}

savitzkyGolay <- function(X, m, p, w, delta.wav) {
  if (is.data.frame(X)) 
    X <- as.matrix(X)
  if (w%%2 != 1) 
    stop("needs an odd filter length w")
  if (p >= w) 
    stop("filter length w should be greater than polynomial order p")
  if (p < m)
    stop("polynomial order p should be geater or equal to differentiation order m")
  gap <- (w - 1)/2
  basis <- outer(-gap:gap, 0:p, "^")
  A <- solve(crossprod(basis, basis), tol = 0) %*% t(basis)
  if (is.matrix(X)) {
    if (w >= ncol(X)) 
      stop("filter length w should be lower than ncol(X)")
    output <- factorial(m) * convCppM(X, A[m + 1, ])
    g <- (w - 1)/2
    colnames(output) <- colnames(X)[(g + 1):(ncol(X) - g)]
    rownames(output) <- rownames(X)
  }
  if (is.vector(X)) {
    if (w >= length(X)) 
      stop("filter length w should be lower than length(X)")
    output <- factorial(m) * convCppV(X, A[m + 1, ])
    g <- (w - 1)/2
    names(output) <- names(X)[(g + 1):(length(X) - g)]
  }
  # scaling
  if (!missing(delta.wav)) 
    output <- output/delta.wav^m
  return(output)
} 

if(!exists('sg.set'))sg.set<-c(previous.name,'','','','','')
ans<-inpboxeeeeee(c('*Matrix Name','*Derivative','*Polynomial','*Window size','*First column','*Last column'),
                  sg.set)
if(!is.null(ans)){
    if(ans[[1]]!=""&ans[[2]]!=""&ans[[3]]!=""&ans[[4]]!=""&ans[[5]]!=""&ans[[6]]!=""){
        previous.name<-ans[[1]]
        sg.set<-ans
        var.trans<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
        #cl_bl<-ncol(var.trans)
        M<-var.trans[,ans[[5]]:ans[[6]],drop=FALSE]
        m<-as.numeric(ans[[2]])
        p<-as.numeric(ans[[3]])
        w<-as.numeric(ans[[4]])
        Z<-savitzkyGolay(X = M,m = m,p = p,w = w)
         
        if(ans[[5]]==1&ans[[6]]==ncol(var.trans)) var.trans<-Z
        if(ans[[5]]==1&ans[[6]]<ncol(var.trans)){
          nc<-as.numeric(ans[[6]])+1
          var.trans<-cbind.data.frame(Z,var.trans[,nc:ncol(var.trans),drop=FALSE])
        }
        if(ans[[5]]>1&ans[[6]]==ncol(var.trans)){
          nc<-as.numeric(ans[[5]])-1
          var.trans<-cbind.data.frame(var.trans[,1:nc,drop=FALSE],Z)
        }
        if(ans[[5]]>1&ans[[6]]<ncol(var.trans)){
          nc<-as.numeric(ans[[5]])-1;nc_1<-as.numeric(ans[[6]])+1
          var.trans<-cbind.data.frame(var.trans[,1:nc,drop=FALSE],Z,var.trans[,nc_1:ncol(var.trans),drop=FALSE])
        }
        
        assign(paste(ans[[1]],'.sg',sep=''),var.trans,envir=.GlobalEnv)
        print(paste('The value is saved in: ',ans[[1]],'.sg',sep=''))
    }else{
        tk_messageBox(type=c("ok"),message="Complete the informations",caption="Input Error")
    }
}

rm_obj<-c('convCppM','convCppV','savitzkyGolay','M','m','p','w','Z','var.trans')
for (o in 1:length(rm_obj))if(exists(rm_obj[o]))rm(list=rm_obj[o])
rm(rm_obj,o)


