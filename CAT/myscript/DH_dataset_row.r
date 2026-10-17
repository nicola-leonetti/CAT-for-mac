# Purpose   : allow to modify a dataset by row. Rows may be extracted, deleted 
#             and used as a column names
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
variable<-gvfib()
M<-variable$value
if((typeof(M)=='list')|(typeof(M)=='double')){
ans<-inpboxcrrrr('Row Number',c(as.character(1:nrow(M)),'name'),'Select:',c('Delete','Extract','Column Names','Copy'))
if(!is.null(ans)){
if(as.numeric(ans[[1]])<=nrow(M)){
nr<-ans[[1]]
ex.row<-unlist(M[nr,])
if(ans[[2]]){
M<-as.data.frame(M[-nr,])
rm(ex.row)}
if(ans[[3]]){
M<-as.data.frame(M[-nr,])
assign('ex.row',ex.row,envir=.GlobalEnv)
print('Row is copied in variable: ex.row')}
if(ans[[4]]){
M<-as.data.frame(M[-nr,])
M<-as.data.frame(M)
colnames(M)<-ex.row
rm(ex.row)}
if(ans[[5]]){
assign('ex.row',ex.row,envir=.GlobalEnv)
print('Row is copied in variable: ex.row')}
assign(variable$name,M,envir=.GlobalEnv)
}else{
if(ans[[3]]|ans[[4]]|ans[[5]]){
assign('vname',names(M),envir=.GlobalEnv)
print('Column Names are copied in vector: vname')
}else{
names(M)<-as.character(1:ncol(M))
assign(variable$name,M,envir=.GlobalEnv)
print('Row Names are deleted')
}
}
assign(variable$name,M,envir=.GlobalEnv)
rm(ans)
}}else{tk_messageBox(type=c("ok"),message='Only Matrix for this Function!',
caption="Input Error")} 
rm(M,variable) 
