# Authors   : R. Leardi, C. Melzi, G. Polotti
#Allows to rotate 3D plots (score, leverage and response plot)

######################################################

Grafici<-which(sapply(ls(), function(x) class(get(x)))=="trellis")
Cloud.3D<-Grafici[which(sapply(names(Grafici), function(x) (get(x)$panel))=="panel.cloud" )]
Wireframe.3D<-Grafici[which(sapply(names(Grafici), function(x) (get(x)$panel))=="panel.wireframe")]


Grafici.3D<-c(Cloud.3D,Wireframe.3D)


ans<-inpboxc(tcombo = "Graph",vcombo = names(Grafici.3D),inp = 0)
if(!is.null(ans)){
graph<-get(names(Grafici.3D)[ans[[1]]])

ans<-inpboxc2(c('Rotation angle x (horizontal)','Rotation angle z (vertical)'),
              as.character(seq(0,350,10)),as.character(seq(0,90,10)),inp = c(4,6))
if(!is.null(ans)){
dev.new(title="3D plot")
print(update(graph,screen=list(z=seq(0,350,10)[ans[[1]]],x=-seq(0,90,10)[ans[[2]]])))
}
rm(graph,Grafici,Grafici.3D,Wireframe.3D,Cloud.3D)}

rm(ans)
