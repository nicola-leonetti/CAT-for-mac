# Purpose   : EDA (Exploratory Data Analysis) plot of a variable 
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 3.3
# Licence   : GPL2.1
#
variable<-gvfib_1()
if(length(variable$input)!=0){
variable.value<-as.matrix(variable$value)
if(sum(is.na(variable.value))!=0)print('>>NA found and ignored<<')
variable.value<-na.omit(variable.value)
nr<-nrow(variable.value)
nc<-ncol(variable.value)
if((nc==1)|(nr==1)){
suppressWarnings(library(StatDA))        # needed for eda plot
vn<-colnames(get(variable$name))[as.numeric(sub("]","",sub(".*\\[,", "", variable$surname)))]
dev.new(title=paste("EDA plot",vn))
edaplot(variable.value,H.freq=FALSE,P.axes=FALSE,P.xlab='',P.ylab='',P.main=paste('EDA Plot',vn))
axis(1, at = NULL, labels = TRUE)
grid();box()
rm(variable.value,nr,nc)}else{
tk_messageBox(type=c("ok"),message='The Variable must be a single vector !',caption="Input Error")}}
