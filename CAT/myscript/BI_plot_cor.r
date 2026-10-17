# Purpose   : Draw an image with the Blue-Red scale to modulate the abosolute
#             value of each component of a correlation matrix.
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 2.5
# Licence   : GPL2.1
#

#require(gplots)
#require(graphics)
suppressWarnings(suppressPackageStartupMessages(library("lattice")))
require(lattice)
source(paste(script.dir,"BI_correlation.r",sep="/"))
if(!is.null(variable)){
n<-ncol(var.cor)
C<-matrix(rep(0,n*n),n,n)
dev.new(title="correlation matrix")
for(i in 1:n){for(j in 1:n){C[i,n-(j-1)]=var.cor[i,j]}}
vc<-colorRampPalette(c("blue","white","red"))(101) 


graph<-levelplot(C,col.regions=vc,at=seq(-1,1,0.02),
          xlab='Variable Index',ylab='Variable Index',
          scales=list(x=list(label=seq(1,n),at=seq(1,n),cex=0.6),
                      y=list(label=rev(seq(1,n)),at=seq(1,n),cex=0.6)));print(graph)


if(n<10){
  trellis.focus("panel", 1, 1, highlight=FALSE)
  for(i in 1:(n-1)){
  panel.abline(h=i+0.5,lty=1,col='black')
  panel.abline(v=i+0.5,lty=1,col='black')
  }
  trellis.unfocus()
  }
rm(n,C,i,j,vc,graph)}
