# Purpose   : write the arithmetic average of a vector/matrix requiring the 
#             name of a variable in the memory through a Input box
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 3.1
# Licence   : GPL2.1
#
variable<-gvfib()
if(length(variable$input)!=0){
	variable$value<-as.matrix(variable$value)
	if(sum(is.na(variable$value))!=0)print('>>NA found and ignored<<')
	# whole matrix calculation if the matrix is indicated by its name only
	if(variable$control==1){
		if(is.numeric(t(variable$value))){
			variable$value<-unlist(variable$value)
			var.mean<-mean(variable$value,na.rm=TRUE)
			assign('var.mean',var.mean,envir=.GlobalEnv)
		    print('The value is saved in: var.mean')
		    print(var.mean)
		}else{
			print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
		}
	}
	# row-wise calculation if the matrix is a row-submatrix
	if(variable$control==2){
		if(is.numeric(t(variable$value))){
			var.mean<-apply(variable$value,2,mean,na.rm=TRUE)
			assign('var.mean',var.mean,envir=.GlobalEnv)
		    print('The value is saved in: var.mean')
		    print(var.mean)
		}else{
			print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
		}
	}
	# column-wise calculation if the matrix is a column-submatrix
	if(variable$control==3){
		if(is.numeric(t(variable$value))){
			var.mean<-apply(variable$value,2,mean,na.rm=TRUE)
			assign('var.mean',var.mean,envir=.GlobalEnv)
			print('The value is saved in: var.mean')
			print(var.mean)
		}else{
			print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
		}
	}
}
rm(variable)
