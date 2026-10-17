# Purpose   : Probability Density plot of a variable
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 2.2
# Licence   : GPL2.1
#
require(graphics)

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
        dev.new(title=paste("Probaility Density Plot",vn))
        plot(density(variable.value),xlab='',ylab='',main=paste('Probability Density Plot',vn))
        grid()
      }else{
        cat <- makevar(ans[[2]])$value
        if(sum(is.na(as.matrix(variable$value)))!=0)cat <- cat[!is.na(as.matrix(variable$value))]
        cat <- as.factor(cat)
        vcolor<-unlist(dovc(as.character(levels(cat))))
        dens.cat <- c(NULL);i=0
        for(lv in levels(cat)){
          i=i+1
          dens.cat[i] <- max(density(variable.value[which(cat==lv)])$y)
        }
        dev.new(title=paste("Probaility Density Plot",vn))
        plot(density(variable.value),xlab='',ylab='',main=paste('Probability Density Plot',vn),type='n',ylim=c (0,max(dens.cat)))
        i=0
        for(lv in levels(cat)){
          i=i+1
          lines(density(variable.value[which(cat==lv)]),col=vcolor[i])
        }
        grid()
        legend("topright", legend=levels(cat), col=vcolor, lty=1,lwd=2,cex=0.6,bty = "n")
        rm(i,cat,dens.cat,lv)
      }
      
      rm(variable.value,nr,nc)}else{
        tk_messageBox(type=c("ok"),message='The Variable must be a single vector !',caption='Input Error')}}
  
  if(exists('vn'))rm(vn)
  if(exists('nr'))rm(nr)
  if(exists('nc'))rm(nc)
}



