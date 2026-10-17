# Authors   : R. Leardi, C. Melzi, G. Polotti
#Allows to rotate 3D plots (score, leverage ande response plot)

######################################################

require(lattice)
Grafici<-which(sapply(ls(), function(x) class(get(x)))=="trellis")
Cloud.3D<-Grafici[which(sapply(names(Grafici), function(x) (get(x)$panel))=="panel.cloud" )]
Wireframe.3D<-Grafici[which(sapply(names(Grafici), function(x) (get(x)$panel))=="panel.wireframe")]


Grafici.3D<-c(Cloud.3D,Wireframe.3D)

ans<-inpboxc(tcombo = "Graph",vcombo = names(Grafici.3D),inp = 0)
if(!is.null(ans)){
 graph<-get(names(Grafici.3D)[ans[[1]]])

ans<-inpboxc(tcombo="Rotation angle z (vertical)",vcombo=seq(0,90,10),inp=6)
if(!is.null(ans)){
dev.new(title="3D plot")

z<-seq(0,360,1)
for (i in z){
  
  Sys.sleep(0.05)
  print(update(graph,screen=list(z=i,x=-seq(0,90,10)[ans[[1]]]))) 
  panel.text(30,15,paste("x =",i),cex=1,col='red')
}




rm(graph,Cloud.3D,Grafici,Grafici.3D,i,Wireframe.3D,z)
}

}

rm(ans)