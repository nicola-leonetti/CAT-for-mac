# Purpose   : histogram of a variable
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 2.2
# Licence   : GPL2.1
#

if(!exists('uni.set')) uni.set<-c(previous.name,'None')
ans<-inpboxee(c('*Select variable (e.g., A[,4])','Category vector (e.g., A[,1])'),uni.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  uni.set<-unlist(ans)
  variable<-makevar(ans[[1]])
  if(length(variable$input)!=0){
    suppressWarnings(library(ggplot2)) 
    library(ggplot2)
    variable.value<-as.matrix(variable$value)
    if(sum(is.na(variable.value))!=0)print('>>NA found and ignored<<')
    variable.value<-na.omit(variable.value)
    nr<-nrow(variable.value)
    nc<-ncol(variable.value)
    vn<-colnames(get(variable$name))[as.numeric(sub("]","",sub(".*\\[,", "", variable$surname)))]
    hs <- hist(variable.value,plot=FALSE)
    if((nc==1)|(nr==1)){
      if(ans[[2]]=='None'){
        dev.new(title=paste("histogram",vn))
        df<-data.frame(x=variable.value)
        gr <- ggplot(df,aes(x=x))+
          geom_histogram(fill  = "white",breaks=hs$breaks,color='black')+
          ggtitle(paste('Histogram',vn)) +
          xlab('') + ylab('Frequency')+
          theme_bw()+
          theme(
            aspect.ratio=1,
            plot.title = element_text(face = "bold", size = 16),
            panel.grid.major = element_line(colour = "white", size = 0.2)
          )
        print(gr)
      }else{
        cat <- makevar(ans[[2]])$value
        if(sum(is.na(as.matrix(variable$value)))!=0)cat <- cat[!is.na(as.matrix(variable$value))]
        cat <- as.factor(cat)
        vcolor<-unlist(dovc(as.character(levels(cat))))
        dev.new(title=paste("histogram",vn))
        df<-data.frame(x=variable.value,Category=cat)
        gr <- ggplot(df,aes(x=x,color=Category))+
          geom_histogram(fill  = "white",breaks=hs$breaks,position='dodge')+
          scale_color_manual(values=vcolor)+
          ggtitle(paste('Histogram',vn)) +
          xlab('') + ylab('Frequency')+
          theme_bw()+
          theme(
            aspect.ratio=1,
            plot.title = element_text(face = "bold", size = 16),
            panel.grid.major = element_line(colour = "white", size = 0.2)
          )
        print(gr)
        rm(cat,vcolor)
      }
      rm(variable.value,nr,nc,hs,df,gr,vn)}else{
        tk_messageBox(type=c("ok"),message='The Variable must be a single vector !',caption="Input Error")}}
}


