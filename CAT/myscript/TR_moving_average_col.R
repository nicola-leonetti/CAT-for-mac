# Purpose   : column moving average
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
moving_average <- function(x, n) {
  filter <- rep(1/n, n)
  stats::filter(x, filter, sides = 2) # sides=2 for centered moving average
}
ma_c <- function(x){
  if(sum(is.na(x))==0){
    return(x[length(x)])
  }else{
    return(x[min(which(is.na(x)))-1]) 
  }
}

if(!exists('cma.set'))cma.set<-c(previous.name,'','','','3')
ans <- inpboxeeee(c('*Matrix name','*First column','*Last column','window size (odd number)'),
                  cma.set)
if(!is.null(ans)){
  n <- as.numeric(ans[[4]])
  if(n %% 2 == 0){
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    tk_messageBox(type=c("ok"),message="window size must be an odd number!",caption="Input Error")
    rm(opt,n,moving_average,ma_c)
    stop()
  }else{
    previous.name<-ans[[1]]
    cma.set<-ans
    var.trans<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
    M<-var.trans[,ans[[2]]:ans[[3]],drop=FALSE]
    if(sum(is.na(M))!=0){
      opt <- options(show.error.messages=FALSE)
      on.exit(options(opt))
      tk_messageBox(type=c("ok"),message="NA found: remove them before evaluation",caption="Input Error")
      rm(ans,var.trans,M,opt,n,moving_average,ma_c)
      stop()
    }else{
      c <- nrow(var.trans)
      r <- (n-1)/2
      for(j in 1:ncol(M)){	
        MA <- matrix(rep(0,c*(r+1)),nrow = (r+1),ncol = c)
        for(i in 1:(r+1)) MA[i,] <- moving_average(M[,j], 2*(i-1)+1)[1:c]
        M[,j]<-apply(MA,2,"ma_c")
      }
      var.trans[,ans[[2]]:ans[[3]]]<-M
      assign(paste(ans[[1]],'.mac',sep=''),var.trans,envir=.GlobalEnv)
      print(paste('The value is saved in: ',ans[[1]],'.mac',sep=''))
      rm(i,j,M,MA,var.trans,n,moving_average,ma_c)
    }
  }
}else{
  rm(moving_average,ma_c)
  stop()
}