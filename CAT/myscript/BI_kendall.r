# Purpose   : make the kendal dispersion of a data set requiring  
#             the name of a variable in the memory through a Input box.
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 2.0
# Licence   : GPL2.1
#
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
var.ken<-cor(variable.value,method='kendall',use = "complete.obs")
print('The value is saved in: var.ken')
print(round(var.ken,3))
rm(variable.value,nr,nc)}else{
tk_messageBox(type=c('ok'),message='Variable must have dimension greater than 2!',caption='Input Error')}}
