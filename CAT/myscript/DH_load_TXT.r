# Purpose   : function to load data.frame from a TXT file with TAB separator 
# Input     : name of new variable, format settings:
#             "." for decimal,
#             header : 1 line with field name
#             file name and path trough a "input box"
# Output    : none directly; a variable tabella with imported data is made in
#             Global enviromental
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.7
# Licence   : GPL2.1
#
require(tools)
require(stringr)
nome_file<-file.choose(new = FALSE)
repeat{
  ans<-inpboxeeeeekk(c('* Matrix Name','* Decimal Separator','* Missing Data','* Skip Top Rows','* Skip Left Columns'),c('Header','Row Names'),
                     c(str_replace_all(file_path_sans_ext(basename(nome_file))," ",""),'.','NA','0','0','TRUE','FALSE'))
  if(is.null(ans))break
  if(!is.null(ans)){
    variable<-NULL
    variable$input<-ans[[1]]
    if(variable$input==make.names(variable$input)){
      oyn<-'yes'
      if(exists(variable$input,envir=.GlobalEnv)){
        oyn<-tk_messageBox(type="yesno",message='Variable Exists.Overwrite ?')
      }
      if(oyn=='yes'){
        variable$value<-read.delim(nome_file,header=as.logical(ans[[6]]),sep="\t",quote="\"",dec=as.character(ans[[2]]),na.strings = as.character(ans[[3]]),fill=TRUE)
        if(as.numeric(ans[[4]])!=0)variable$value<-variable$value[-(1:as.numeric(ans[[5]])),]
        if(as.numeric(ans[[5]])!=0)variable$value<-variable$value[,-(1:as.numeric(ans[[5]]))]
        if(as.logical(ans[[7]])){
          rownames(variable$value)<-variable$value[,1]
          variable$value<-variable$value[,-1]
        }
        variable$existance<-'TRUE'
        variable$control<-1
        variable$name<-variable$input
        assign('variable',variable,envir=.GlobalEnv)
        assign(variable$name,variable$value,envir=.GlobalEnv)
        assign('previous.name',variable$input,envir=.GlobalEnv)
        nr_<-nrow(variable$value)
        nc_<-ncol(variable$value)
        print(paste(nr_*nc_,' Data loaded: ',nr_,' Rows & ',nc_,' Columns',sep=''),quote=FALSE)
      }
      rm(ans,nome_file,oyn,variable,nr_,nc_)
      break
    }
    tk_messageBox(type=c("ok"),message='Unacceptable matrix name',caption="Input Error")
  }
  }







