# Authors   : R. Leardi, C. Melzi, G. Polotti
suppressWarnings(suppressPackageStartupMessages(library("grid")))
suppressWarnings(suppressPackageStartupMessages(library("cairoDevice")))
suppressWarnings(suppressPackageStartupMessages(library("playwith")))

library (grid)
library(cairoDevice)
library(playwith)

Grafici<-which(sapply(ls(), function(x) class(get(x)))=="trellis")
Grafici<-Grafici[attr(Grafici,"names")!="graph"]
Grafici<-Grafici[!substr(attr(Grafici,"names"), nchar(attr(Grafici,"names"))-2, nchar(attr(Grafici,"names")))==".3D"]

ans<-inpboxck(tcombo = "Plot in the list",vcombo = names(Grafici),tcheck = "Different plot",
              vinp = c(0,"FALSE"))
if(!is.null(ans)){
  if(ans[[2]]){require(zoom)
  if(dev.cur()>=2){
    print('Close the Plot Window after you have done !',quote=FALSE)
    zm()
  }else{
    tk_messageBox(type=c("ok"),message="A plot must be drawn first !",caption="Input Error")}
  }else{
     graph<-get(names(Grafici)[ans[[1]]])
  playwith(graph) 
  }
 rm(Grafici) 
}
rm(ans)

