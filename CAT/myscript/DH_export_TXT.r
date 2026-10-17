# Purpose   : function to export a data.frame from a CSV file. Format :
# Input     :  file name and path trough a "input box", export settings:
#             ";" delimiter,
#             "." for decimal,
#             "NA" for missing values,
#             "" for string
#             header : 1 line with field name
# Output    : CSV file with the name of the variable in the working directory
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.4
# Licence   : GPL2.1
#
ans<-inpboxeeekk(c('* Matrix Name','* Decimal Separator','* Missing Data'),c('Header','Row Names'),c(paste(previous.name),'.','NA','TRUE','FALSE'))
variable<-NULL
if(!is.null(ans))variable<-makevar(ans[[1]])
if(!is.null(variable)){
	M<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
	if((as.logical(ans[[4]]))&(as.logical(ans[[5]]))){
		mt<-dimnames(M)[[2]]
		if(is.null(mt))mt<-as.character(1:ncol(M))
		mt<-matrix(c(' ',mt),1,ncol(M)+1)
		write.table(mt,paste(variable$name,'.txt',sep=''),sep="\t",quote=TRUE,row.names=FALSE,col.names=FALSE)
		write.table(M,paste(variable$name,'.txt',sep=''),sep="\t",quote=TRUE,append=TRUE,dec=as.character(ans[[2]]),na=as.character(ans[[3]]),row.names=TRUE,col.names=FALSE)
		rm(mt)
	}
	if((as.logical(ans[[4]]))&(!as.logical(ans[[5]]))){
		write.table(M,paste(variable$name,'.txt',sep=''),sep="\t",quote=TRUE,dec=as.character(ans[[2]]),na=as.character(ans[[3]]),row.names=FALSE,col.names=TRUE)
	}
	if((!as.logical(ans[[4]]))&(as.logical(ans[[5]]))){
		write.table(M,paste(variable$name,'.txt',sep=''),sep="\t",quote=TRUE,dec=as.character(ans[[2]]),na=as.character(ans[[3]]),row.names=TRUE,col.names=FALSE)
	}
	if((!as.logical(ans[[4]]))&(!as.logical(ans[[5]]))){
		write.table(M,paste(variable$name,'.txt',sep=''),sep="\t",quote=TRUE,dec=as.character(ans[[2]]),na=as.character(ans[[3]]),row.names=as.logical(ans[[5]]),col.names=as.logical(ans[[4]]))
	}
	assign('previous.name',variable$name,envir=.GlobalEnv,inherits=FALSE)
rm(M)}else{
	tk_messageBox(type=c("ok"),message='The Variable does not exist!',caption="Input Error")
}
