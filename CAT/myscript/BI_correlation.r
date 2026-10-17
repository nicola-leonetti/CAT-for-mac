# Authors   : R. Leardi, C. Melzi, G. Polotti
suppressWarnings(suppressPackageStartupMessages(library("Hmisc")))
require(Hmisc)

variable<-gvfib()
if(length(variable$input)!=0){
  variable.value<-as.matrix(variable$value)
  if(sum(is.na(variable.value))!=0){
    #print('>>NA found: remove them before evaluation<<')
    #variable<-NULL
    print(paste('Removed',sum(apply(is.na(variable.value),1,sum)[apply(is.na(variable.value),1,sum)==1]),'rows: NA found'))
    variable.value<-variable.value[apply(is.na(variable.value),1,sum)==0,]
  }
    nr<-nrow(variable.value)
    nc<-ncol(variable.value)
    if((nc>=2)&(nr>=2)){
      #var.cor<-cor(variable$value)
      var.cor<-rcorr(as.matrix(variable$value))
      print('The correlation matrix and the p values are saved in: var.cor')
      cat("    ", "\n")
      cat("correlation matrix", "\n")
      # print(round(as.dist(var.cor$r,upper = TRUE),3))
      print(round(var.cor$r,3))
      cat("    ", "\n")
      cat("p-values", "\n")
      # print(round(as.dist(var.cor$P,upper = TRUE),3))
      print(round(var.cor$P,3))
      var.cor <- var.cor$r
      rm(variable.value,nr,nc)}else{tk_messageBox(type=c('ok'),
                                                  message='Variable must have dimension greater than 2!',caption='Input Error')}}



