# Purpose   : allow to modify a dataset by column.Columns may be extracted,deleted 
#             and used as a row names
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
variable<-gvfib()
M<-variable$value
if((typeof(M)=='list')|(typeof(M)=='double')){
ans<-inpboxcrrrr('Column Number',c(as.character(1:ncol(M)),'name'),'Select:',c('Delete','Extract','Column Names','Copy'))
if(!is.null(ans)){
if(as.numeric(ans[[1]])<=ncol(M)){
nc<-ans[[1]]
ex.col<-unlist(M[,nc])
if(ans[[2]]){
M<-as.data.frame(M[,-nc])
rm(ex.col)}
if(ans[[3]]){
M<-as.data.frame(M[,-nc])
assign('ex.col',ex.col,envir=.GlobalEnv)
print('Column is copied in variable: ex.col')}
if(ans[[4]]){
M<-as.data.frame(M[,-nc])
M<-as.data.frame(M)
rownames(M)<-ex.col
rm(ex.col)}
if(ans[[5]]){
assign('ex.col',ex.col,envir=.GlobalEnv)
print('Column is copied in variable: ex.col')}
rm(nc)
}else{
if(ans[[3]]|ans[[4]]|ans[[5]]){
assign('vname',rownames(M),envir=.GlobalEnv)
print('Row Names are copied in vector: vname')
}else{
row.names(M)<-NULL
assign(variable$name,M,envir=.GlobalEnv)
print('Row Names are deleted')
}
}
assign(variable$name,M,envir=.GlobalEnv)
rm(ans)
}}else{tk_messageBox(type=c("ok"),message='Only Matrix for this Function!',
caption="Input Error")}
rm(M,variable)
