# Purpose   : write a summary (mean,median,quartiles,minimum,maximum) of a 
#             data set requid to the user by a Input Box
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 3.0
# Licence   : GPL2.1
variable<-gvfib()
if(length(variable$input)!=0){
		if(sum(is.na(variable$value))!=0)print('>>NA found and ignored<<')
		# whole matrix calculation if the matrix is indicated by its name only
		if(variable$control==1){
			if(is.numeric(t(variable$value))){
				print.table(summary(unlist(variable$value)))
			}else{
				print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
			}
		}
		# row-wise calculation if the matrix is a row-submatrix
		if(variable$control==2){
			if(is.numeric(t(variable$value))){
				print.table(summary (variable$value))
			}else{
				print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
			}
		}
		# column-wise calculation if the matrix is a column-submatrix
		if(variable$control==3){print.table(summary(variable$value))}
}
