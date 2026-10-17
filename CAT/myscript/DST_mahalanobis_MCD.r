# Purpose   : write the robust Mahalanobis distance of a matrix requiring the 
#             name of a variable in the memory through a Input box
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 3.0
# Licence   : GPL2.1
#
variable<-gvfib()
if(!is.null(variable)){
M<-as.matrix(variable$value)
if((nrow(M)!=1)&(ncol(M)!=1)){
v.mcd<-covMcd(M)
var.dist<-as.matrix(sqrt(mahalanobis(M,v.mcd$center,cov=v.mcd$cov)))
print('The value is saved in: var.dist')
print(var.dist)
rm(v.mcd)}else{tk_messageBox(type=c("ok"),
message='Variable must have both dimensions greater than 2!',caption="Input Error")}
rm(M)}
