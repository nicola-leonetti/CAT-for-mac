# Purpose   : Evaluate a 3W PCA model from an unfolded dataset and make a PCA3W object.
# Input matrix x must be a "double" unfolded matrix.
# With n objects, v variables and c conditions it is a n*c,v matrix:
# Conditions can be different observations, different years, different
# replicates, different operators, ...
# The order of the rows has to be the following:
# obj. 1, cond. 1
# obj. 2, cond. 1
# ...............
# obj. n, cond. 1
# obj. 2, cond. 2
# ...............
# obj. n, cond. c
# It works with 2 components for each dimension
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
suppressWarnings(suppressPackageStartupMessages(library("Rcpp")))
require(pcaMethods)
require(matrixcalc)
require(expm)
ans<-inpboxeec(c('*Matrix Name (e.g., A[,3:10])','*Number of conditions','*Scaling Method'),
c('none','j-scaling','jk-scaling'),c(previous.name,''))
if(!is.null(ans) & ans[[2]]!=""){
PCA3W<-list()
variable<-NULL
variable<-makevar(ans[[1]])
if((typeof(variable$value)=='double')|(typeof(variable$value)=='list')){
M<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
M<-as.matrix(M)
if(is.numeric(M)){
  nNA<-sum(is.na(M))
  if(nNA>0){
    mess<-paste('program aborted - ',as.character(nNA),'missing data present')
    tk_messageBox(type=c("ok"),message=mess,caption="Input Error")
  }else{
    Mo<-NULL
    Mv<-NULL
    Mc<-NULL
    if(as.numeric(ans[[3]])==2)M<-scale(M,center=TRUE,scale=TRUE)
    oc<-nrow(M)
    v<-ncol(M)
    c_<-as.numeric(ans[[2]])
    maxci<-20 # fix number of maximum iterations
    o<-oc/c_
    if(o%%1==0){
      for(i in 1:c_)Mo<-cbind(Mo,M[(i-1)*o+1:o,])
      if(as.numeric(ans[[3]])==3){
        Mo<-scale(Mo,center=TRUE,scale=TRUE)
        M<-NULL
        for(i in 1:c_)M<-rbind(M,Mo[,(i-1)*v+1:v])}
      for(i in 1:c_)Mv<-cbind(Mv,t(Mo[,(i-1)*v+1:v]))
      for(i in 1:c_){
        Mt<-NULL
        for(j in 1:o)Mt<-cbind(Mt,t(Mv[,(i-1)*o+j]))
        Mc<-rbind(Mc,Mt)}
      varin<-sum(sum(M^2))
      cat('Note : Two Components per Mode will be computed','\n')
      cat('','\n')
      cat('Cumulative % variance explained after unfolding:','\n')
      cat('','\n')
      #print(paste('Initial variance',varin),quote=FALSE)
      res<-pca(t(Mo),method="nipals",nPcs=2,scale="none",center=FALSE)
      lo<-t(loadings(res))
      cat('Mode of objects:','\n')
      cat(round(R2cum(res)*100,2),'\n')
      cat('','\n')
        
      res<-pca(t(Mv),method="nipals",nPcs=2,scale="none",center=FALSE)
      lv<-t(loadings(res))
      cat('Mode of variables:','\n')
      cat(round(R2cum(res)*100,2),'\n')
      cat('','\n')
      
      
      res<-pca(t(Mc),method="nipals",nPcs=2,scale="none",center=FALSE)
      lc<-t(loadings(res))
      cat('Mode of conditions:','\n')
      cat(round(R2cum(res)*100,2),'\n')
      cat('','\n')
      
      
      delta<-1
      ci<-0
      perc<-NULL
      while((delta>0.001)&(ci<maxci+1)){
        if (ci>0){
          m<-Mo%*%((t(lc)%*%lc)%x%(t(lv)%*%lv))%*%t(Mo)
          lo<-t(m%*%t(lo)%*%suppressMessages(sqrtm(matrix.inverse(lo%*%m%*%m%*%t(lo)))))
          m<-Mv%*%((t(lc)%*%lc)%x%(t(lo)%*%lo))%*%t(Mv)
          lv<-t(m%*%t(lv)%*%suppressMessages(sqrtm(matrix.inverse(lv%*%m%*%m%*%t(lv)))))
          m<-Mc%*%((t(lo)%*%lo)%x%(t(lv)%*%lv))%*%t(Mc)
          lc<-t(m%*%t(lc)%*%suppressMessages(sqrtm(matrix.inverse(lc%*%m%*%m%*%t(lc)))))}
        G<-lo%*%Mo%*%(t(lc)%x%t(lv))
        nG<-which.max(apply(abs(t(G)),2,max))
        if(nG==2){
          lo2=lo
          lo2[1,]<-lo[2,]
          lo2[2,]<-lo[1,]
          lo<-lo2
          rm(lo2)
          G<-lo%*%Mo%*%(t(lc)%x%t(lv))}
        nG<-which.max(apply(abs(G),2,max))
        if((nG==2)|(nG==4)){
          lc2<-lc
          lc2[1,]<-lc[2,]
          lc2[2,]<-lc[1,]
          lc<-lc2
          rm(lc2)
          G<-lo%*%Mo%*%(t(lc)%x%t(lv))}
        if(nG>2){
          lv2<-lv
          lv2[1,]<-lv[2,]
          lv2[2,]<-lv[1,]
          lv<-lv2
          rm(lv2)}
        #  print(ci)
        G<-lo%*%Mo%*%(t(lc)%x%t(lv))
        percvarexp<-sum(sum(G^2))/varin*100
        perc<-c(perc,percvarexp)
        if(ci>0)delta<-perc[ci+1]-perc[ci]
        ci<-ci+1}
      cat('','\n')
      cat(paste('% variance explained by the Tucker model:',round(percvarexp,2)),'\n')
      cat('','\n')
      cat('Core Matrix','\n')
      row.names(G) <- c('1,,','2,,')
      colnames(G) <- c(',1,1',     ' ,2,1 ',    ' ,1,2 ',     ',2,2')
      print(round(G,5))
      cat('','\n')
      G2<-matrix(rep(0,8),2,4)
      fi<-(G[1,1]^2+G[2,4]^2)/sum(sum(G^2))
      fin<-0
      fimax<-fi
      while(fimax>fin){
        fin<-fimax
        # print('Rotation of objects',quote=FALSE)
        rotb<-0
        for(rot in -90:90){
          RM<-matrix(c(cos(rot*pi/180),sin(rot*pi/180),-sin(rot*pi/180),cos(rot*pi/180)),2,2)
          lo2<-RM%*%lo
          G2<-lo2%*%Mo%*%(t(lc)%x%t(lv))
          fi2<-(G2[1,1]^2+G2[2,4]^2)/sum(sum(G2^2))
          if(fi2>fimax){
            lob<-lo2
            fimax<-fi2
            rotb<-rot}}
        if(rotb!=0){
          #  print(rotb,quote=FALSE)
          #  print(fimax,quote=FALSE)
          lo<-lob}
        #  print('Rotation of variables',quote=FALSE)
        rotb<-0
        for(rot in -90:90){
          RM<-matrix(c(cos(rot*pi/180),sin(rot*pi/180),-sin(rot*pi/180),cos(rot*pi/180)),2,2)
          lv2<-RM%*%lv
          G2<-lo%*%Mo%*%(t(lc)%x%t(lv2))
          fi2<-(G2[1,1]^2+G2[2,4]^2)/sum(sum(G2^2))
          if(fi2>fimax){
            lvb<-lv2
            fimax<-fi2
            rotb<-rot}}
        if(rotb!=0){
          #   print(rotb,quote=FALSE)
          #   print(fimax,quote=FALSE)
          lv<-lvb}
        #  print('Rotation of conditions',quote=FALSE)
        rotb<-0
        for(rot in -90:90){
          RM<-matrix(c(cos(rot*pi/180),sin(rot*pi/180),-sin(rot*pi/180),cos(rot*pi/180)),2,2)
          lc2<-RM%*%lc
          G2<-lo%*%Mo%*%(t(lc2)%x%t(lv))
          fi2<-(G2[1,1]^2+G2[2,4]^2)/sum(sum(G2^2))
          if(fi2>fimax){
            lcb<-lc2
            fimax<-fi2
            rotb<-rot}}
        if(rotb!=0){
          #   print(rotb,quote=FALSE)
          #   print(fimax,quote=FALSE)
          lc<-lcb}}
      nc<-nrow(lo)
      for(k in 1:nc){
        mm<-mean(lo[k,])
        mt<-0
        for(i in 1:v){
          ml<-0
          for(j in 1:o)ml=ml+mean(M[seq(j,oc,o),i])%*%(lo[k,j]-mm)
          mt=mt+ml%*%lv[k,i]}
        if(mt<0)lo[k,]<--lo[k,]
        mm<-mean(lc[k,])
        mt<-0
        for(i in 1:v){
          ml<-0
          for(j in 1:c_){
            ml=ml+mean(M[((j-1)*o+1):(j*o),i])%*%(lc[k,j]-mm)}
          mt=mt+ml%*%lv[k,i]}
        if(mt<0)lc[k,]<--lc[k,]}
      G<-lo%*%Mo%*%(t(lc)%x%t(lv))
      nG<-which.max(max(abs(t(G))))
      if(nG==2){
        lo2<-lo
        lo2[1,]<-lo[2,]
        lo2[2,]<-lo[1,]
        lo<-lo2
        rm(lo2)
        G<-lo%*%Mo%*%(t(lc)%x%t(lv))}
      nG<-which.max(max(abs(G)))
      if((nG==2)|(nG==4)){
        lc2<-lc
        lc2[1,]<-lc[2,]
        lc2[2,]<-lc[1,]
        lc<-lc2
        rm(lc2)}
      if(nG>2){
        lv2<-lv
        lv2[1,]<-lv[2,]
        lv2[2,]<-lv[1,]
        lv<-lv2
        rm(lv2)}
          
      cat('Superdiagonalized Core Matrix',"\n")
      # print((G,digit=5)))
      row.names(G) <- c('1,,','2,,')
      colnames(G) <- c(',1,1',     ' ,2,1 ',    ' ,1,2 ',     ',2,2')
      print(round(G,5))
      
      cat(" ","\n")
      cat('Superdiagonalized Core Matrix (% explained variance)',"\n")
      # print('      ,1,1      ,2,1      ,1,2      ,2,2',quote=FALSE)
      G.perc <- round(G^2*percvarexp/sum(G^2),2)
      row.names(G.perc) <- c('1,,','2,,')
      colnames(G.perc) <- c(',1,1',     ' ,2,1 ',    ' ,1,2 ',     ',2,2')
      print(G.perc)

      RMM<-matrix(rep(0,o*c_*v),o*c_,v)
      for(i in 1:o){
        for(j in 1:v){
          for(k in 1:c_){
            rv=   lo[1,i]%*%lv[1,j]%*%lc[1,k]%*%G[1,1]
            rv=rv+lo[1,i]%*%lv[2,j]%*%lc[1,k]%*%G[1,2]
            rv=rv+lo[1,i]%*%lv[1,j]%*%lc[2,k]%*%G[1,3]
            rv=rv+lo[1,i]%*%lv[2,j]%*%lc[2,k]%*%G[1,4]
            rv=rv+lo[2,i]%*%lv[1,j]%*%lc[1,k]%*%G[2,1]
            rv=rv+lo[2,i]%*%lv[2,j]%*%lc[1,k]%*%G[2,2]
            rv=rv+lo[2,i]%*%lv[1,j]%*%lc[2,k]%*%G[2,3]
            rv=rv+lo[2,i]%*%lv[2,j]%*%lc[2,k]%*%G[2,4]
            RMM[o*(k-1)+i,j]<-rv}}}
      dis<-(RMM-M)^2
      # saving data
      PCA3W$dataset<-M
      PCA3W$limits<-c(c_,o,v)
      PCA3W$G<-G
      PCA3W$lo<-lo
      PCA3W$lv<-lv
      PCA3W$lc<-lc
      PCA3W$var<-perc
      PCA3W$dis<-dis
    }else{
      print('the number of rows is not a multiple of the number of declared conditions')
    }
  }
}else{
  print('matrix containing non numerical values')
}
}else{
  tk_messageBox(type=c("ok"),message='Wrong Dataset Dimension!',caption="Input Error")}
}

if(exists('M'))rm(M)
if(exists('Mo'))rm(Mo)
if(exists('Mc'))rm(Mc)
if(exists('Mv'))rm(Mv)
if(exists('Mt'))rm(Mt)
if(exists('lo'))rm(lo)
if(exists('lv'))rm(lv)
if(exists('lc'))rm(lc)
if(exists('lc2'))rm(lc2)
if(exists('lo2'))rm(lo2)
if(exists('lv2'))rm(lv2)
if(exists('lcb'))rm(lcb)
if(exists('lob'))rm(lob)
if(exists('lvb'))rm(lvb)
if(exists('m'))rm(m)
if(exists('nNA'))rm(nNA)
if(exists('rv'))rm(rv)
if(exists('mt'))rm(mt)
if(exists('ml'))rm(ml)
if(exists('mm'))rm(mm)
if(exists('G'))rm(G)
if(exists('G.perc'))rm(G.perc)
if(exists('nG'))rm(nG)
if(exists('rot'))rm(rot)
if(exists('rotb'))rm(rotb)
if(exists('fi'))rm(fi)
if(exists('fin'))rm(fin)
if(exists('fimax'))rm(fimax)
if(exists('RM'))rm(RM)
if(exists('RMM'))rm(RMM)
if(exists('res'))rm(res)
if(exists('o'))rm(o)
if(exists('c_'))rm(c_)
if(exists('v'))rm(v)
if(exists('oc'))rm(oc)
if(exists('maxci'))rm(maxci)
if(exists('ci'))rm(ci)
if(exists('varin'))rm(varin)
if(exists('G2'))rm(G2)
if(exists('fi2'))rm(fi2)
if(exists('nc'))rm(nc)
if(exists('i'))rm(i)
if(exists('j'))rm(j)
if(exists('k'))rm(k)
if(exists('perc'))rm(perc)
if(exists('percvarexp'))rm(percvarexp)
if(exists('delta'))rm(delta)
if(exists('dis'))rm(dis)
if(exists('ans'))rm(ans)
if(exists('variable'))rm(variable)


