# Authors   : R. Leardi, C. Melzi, G. Polotti
a=as.numeric(Sys.time())
set.seed(a)
variable<-gvfib()
if(!is.null(variable)){
                 M<-as.data.frame(variable$value)
                 M.rnd=M[sample(nrow(M)),]
                 M.rnd<-as.data.frame(M.rnd)
                 txt=paste(variable$name,".rnd",sep="")
                 txt1=paste(txt,"=","M.rnd")
                 eval(parse(text=txt1))
                 print(paste('The value is saved in:',txt))
                 rm(M,M.rnd,txt,txt1,a)}





