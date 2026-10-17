# Purpose   : export a picture from the Exploratory tool
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.0
# Licence   : GPL2.1
#
ans<-inpboxcrrr('Display',1:length(displays(GOBI)),'Format :',c('png','bmp','pdf'))
nplot<-as.numeric(ans[[1]])
print(nplot)
if(ans[[2]])fplot<-'png'
if(ans[[3]])fplot<-'bmp'
if(ans[[4]])fplot<-'pdf'
vd<-displays(ggobi_get())
ggobi_display_save_picture(display=vd[[nplot]],path=paste('ggobi',fplot,sep='.'),
filetype=fplot,plot.only=TRUE)
rm(ans,nplot,fplot)
