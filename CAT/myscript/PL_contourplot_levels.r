# Authors   : R. Leardi, C. Melzi, G. Polotti
#Allows to change label and line color to contour plots

Grafici<-which(sapply(ls(), function(x) class(get(x)))=="trellis")
Grafici.contour<-Grafici[which(sapply(names(Grafici), function(x) (get(x)$panel))=="panel.contourplot")]

ans<-inpboxc(tcombo = "Graph",vcombo = names(Grafici.contour),inp = 0)
if(!is.null(ans)){
 graph<-get(names(Grafici.contour)[ans[[1]]])
 # ans<-inpboxe('*Levels (e.g. 50,55,58)',inp="",title='Input')
 ans<-inpboxek('*Levels (e.g. 50,55,58)','Plot editing?',vinp=c('','FALSE'),title='Input')
 if(!is.null(ans)){
   if(!ans[[2]]){
     win.graph()
     print(update(graph,at=as.numeric(unlist(str_split(ans[[1]],',')))))
     rm(graph,Grafici,Grafici.contour)
   }else{
     col<-c("black","red","green","blue","brown","cyan","orange","pink","yellow")
     fs <- c(0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5)
     lv <- as.numeric(unlist(str_split(ans[[1]],',')))
     ans<-inpboxc3(c('Lines','Labels', 'Font size'),
                   col,col,fs,inp = c(0,0,5))
     if(!is.null(ans)){
       win.graph()
       print(update(graph,at=lv,
                    col=col[ans[[1]]],labels=list(cex=fs[ans[[3]]],col=col[[ans[[2]]]]))) 
     }
     rm(graph,col,fs,Grafici,Grafici.contour,lv) 
     }
   }
}
rm(ans)

