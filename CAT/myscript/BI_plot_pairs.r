# Purpose   : make a pairs plot of a matrix (min dim 2 max dim 7) requiring the 
#             name of a variable in the memory through a Input box
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 2.0
# Licence   : GPL2.1
#
variable<-gvfib()
if(length(variable$input)!=0){
variable.value<-as.matrix(variable$value)
nr<-nrow(variable.value)
nc<-ncol(variable.value)
if(((nc>=2)&(nc<=7))|((nr>=2)&(nr<=7))){
dev.new(title="pairs")
pairs(variable.value)
rm(variable.value,nr,nc)}else{
tk_messageBox(type=c('ok'),message='The number of selected variables must be in the range 2-7',caption='Input Error')}}
