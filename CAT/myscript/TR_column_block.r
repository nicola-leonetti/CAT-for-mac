# Purpose   : column autoscaling followed by division by the square root of the number of columns in each block
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: CM
# Version   : 1.0
# Licence   : GPL2.1
#
variable<-gvfib()
if(!is.null(variable)){
  ans<-gnfib(paste('*Number of blocks? ',sep=''),'2')
  if (!is.null(ans)){
    n_blk<-as.numeric(ans)
    var.trans<-variable$value
    for(i in 1:n_blk){
      ans<-gnfib(paste('*First and last column of block ',i,'? (e.g., "3,10")',sep=''))
      if(ans!=""){
        bc_<-as.numeric(unlist(strsplit(ans,",")))
        M<-var.trans[,bc_[1]:bc_[2],drop=FALSE]
        c<-matrix(sqrt(dim(M)[2]),dim(M)[1],dim(M)[2])
        var.trans[,bc_[1]:bc_[2]]<-scale(M,center=TRUE,scale=TRUE)/c
      }else{
        stop(paste("Missing columns of block ",i,"!",sep=""),call. = FALSE)
      }
    }
    assign(paste(variable$name,'.blsc',sep=''),var.trans,envir=.GlobalEnv)
    rm(M,var.trans,n_blk,ans,bc_,i)
    print(paste('The value is saved in: ',variable$name,'.blsc',sep=''))
  }
  rm(variable)
}




