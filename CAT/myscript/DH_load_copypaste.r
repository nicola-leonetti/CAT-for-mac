# Authors   : R. Leardi, C. Melzi, G. Polotti
require(tcltk2)

repeat{
  nl<-inpboxe(title="Input Choice", vlabel="*Matrix name",inp="")
  if(is.null(nl))break
  if(!is.null(nl)){
    new.name<-nl[[1]]
    if(new.name==make.names(new.name)){
      nr<-1
      nc<-1
      oyn<-'yes'
      if(exists(new.name,envir=.GlobalEnv)){
        oyn<-tk_messageBox(type="yesno",message='Variable Exists.Overwrite ?')}
      if(oyn=='yes'){
        ._M<-matrix(rep(0,nr*nc),nr,nc)
        ._M<-as.data.frame(._M)
        ._M<-dfedit(._M)
        assign(new.name,._M,envir=.GlobalEnv)
        print(paste(new.name,'created'),quote=FALSE ) 
        nr<-nrow(._M)
        nc<-ncol(._M)
        print(paste(nr*nc,' Data loaded: ',nr,' Rows & ',nc,' Columns',sep=''),quote=FALSE)
        assign('previous.name',new.name,envir=.GlobalEnv)}
      if(exists("._M"))rm(._M)
      if(exists("nc"))rm(nc)
      if(exists("new.name"))rm(new.name)
      if(exists("nl"))rm(nl)
      if(exists("nr"))rm(nr)
      if(exists("oyn"))rm(oyn)
      break
    }
    tk_messageBox(type=c("ok"),message='Unacceptable matrix name',caption="Input Error")
}
}
