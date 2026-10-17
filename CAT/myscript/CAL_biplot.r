# Purpose   : make biplot graph for PLS object  
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.0
# Licence   : GPL2.1
#
if(exists("PLS",envir=.GlobalEnv)){
ans<-inpboxeek(c('*Component on x-axis','*Component on y-axis'),'Arrows',c(1,2,'FALSE'))
if(!is.null(ans)){
dev.new(title="biplot")
biplot(PLS$res,comps=as.numeric(ans[[1]]):as.numeric(ans[[2]]),var.axes=as.logical(ans[[3]]))
grid()}}else{tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCR/PLS1/PLS2!',
caption="Input Error")}
