# Purpose   : function to load data.frame from a excell xls file.
#             Sheet n. integer number of the sheet that has to be read
#             Skip n.  number of text lines that will be skipped
# Input     : name of new variable; file name and path trough a "input box"
# Output    :  a variable tabella with imported data is made in
#             Global enviroment
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.5
# Licence   : GPL2.1
#
require(tools)
suppressWarnings(suppressPackageStartupMessages(library("readxl")))
require(readxl)
require(stringr)
nome_file<-file.choose(new = FALSE)
repeat{
  ans<-inpboxeeeekk(c('* Matrix Name','* Sheet n.','* Skip Top Rows','* Skip Left Columns'),
                    c('Header','Row Names'),
                    c(str_replace_all(file_path_sans_ext(basename(nome_file))," ",""),'1','0','0','TRUE','FALSE'))
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
        nome_file<-normalizePath(nome_file,winslash="/")
        variable$value<-read_excel(nome_file,sheet=as.numeric(ans[[2]]),skip=as.numeric(ans[[3]]),
                                   col_names=as.logical(ans[[5]]))
        
        if(!as.logical(ans[[5]]))colnames(variable$value) <- 1:length(colnames(variable$value))
        
        
        suppressWarnings(colnames(variable$value)[!is.na(as.numeric(colnames(variable$value)))]
                         <-as.numeric(colnames(variable$value)[!is.na(as.numeric(colnames(variable$value)))]))
        setwd(working.dir)
        if(as.numeric(ans[[4]])!=0)variable$value<-variable$value[,-(1:as.numeric(ans[[4]]))]
        if(as.logical(ans[[6]])){
          rownames(variable$value)<-variable$value[,1]
          variable$value<-variable$value[,-1]
        }
        variable$existance<-'TRUE'
        variable$control<-1
        variable$name<-variable$input
        assign('variable',variable,envir=.GlobalEnv)
        assign(variable$name,variable$value,envir=.GlobalEnv)
        assign('previous.name',variable$input,envir=.GlobalEnv)

        print(paste(previous.name,'created'),quote=FALSE ) 


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




