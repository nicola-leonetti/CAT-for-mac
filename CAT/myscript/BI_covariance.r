# Purpose   : make the covariance matrix of a data set requiring  
#             the name of a variable in the memory through a Input box.
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.4
# Licence   : GPL2.1
#
variable<-gvfib()
if(length(variable$input)!=0){
variable.value<-as.matrix(variable$value)
if(sum(is.na(variable.value))!=0){
	# print('>>NA found: remove them before evaluation<<')
	# variable<-NULL
  print(paste('Removed',sum(apply(is.na(variable.value),1,sum)[apply(is.na(variable.value),1,sum)==1]),'rows: NA found'))
  #variable.value<-variable.value[apply(is.na(variable.value),1,sum)==0,]
variable.value<-na.omit(variable.value)

}
nr<-nrow(variable.value)
nc<-ncol(variable.value)
if((nc>=2)&(nr>=2)){
var.cov<-cov(variable.value)
print('The value is saved in: var.cov')
print(round(var.cov,3))
rm(variable.value,nr,nc)}else{tk_messageBox(type=c("ok"),
message='Variable must have dimension greater than 2!',caption='Input Error')}}
