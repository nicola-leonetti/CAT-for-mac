# Purpose   : Box and Whiskers plot of a variable
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 3.2
# Licence   : GPL2.1
#

if(!exists('uni.set')) uni.set<-c(previous.name,'None')
ans<-inpboxee(c('*Select variable (e.g., A[,4])','Category vector (e.g., A[,1])'),uni.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  uni.set<-unlist(ans)
  variable<-makevar(ans[[1]])
  if(length(variable$input)!=0){
    variable.value<-as.matrix(variable$value)
    if(sum(is.na(variable.value))!=0)print('>>NA found and ignored<<')
    variable.value<-na.omit(variable.value)
    nr<-nrow(variable.value)
    nc<-ncol(variable.value)
    vn<-colnames(get(variable$name))[as.numeric(sub("]","",sub(".*\\[,", "", variable$surname)))]
    if((nc==1)|(nr==1)){
      if(ans[[2]]=='None'){
        dev.new(title=paste("box and whiskers plot",vn))
        boxplot(variable.value,xlab='',ylab='',main=paste('Box and Whiskers Plot',vn),horizontal=TRUE)
        grid()
      }else{
        cat <- makevar(ans[[2]])$value
        if(sum(is.na(as.matrix(variable$value)))!=0)cat <- cat[!is.na(as.matrix(variable$value))]
        cat <- as.factor(cat)
        vcolor<-unlist(dovc(as.character(levels(cat))))
        dev.new(title=paste("box and whiskers plot",vn))
        boxplot(variable.value~cat,xlab='',ylab='',pars  =  list(yaxt = "n"),
                main=paste('Box and Whiskers Plot',vn),horizontal=TRUE,
                las = 1,border=vcolor)
        axis(2, at=c(1:length(levels(cat))), labels = FALSE)
        text(y = c(1:length(levels(cat)))+0.1, par("usr")[1], labels = levels(cat),
             srt = 45, pos = 2, xpd = TRUE)
        grid()
        rm(cat)
      }
      
      rm(variable.value,nr,nc)}else{
        tk_messageBox(type=c("ok"),message='The Variable must be a single vector !',caption="Input Error")}}
  
  if(exists('vn'))rm(vn)
  if(exists('nr'))rm(nr)
  if(exists('nc'))rm(nc)
}



