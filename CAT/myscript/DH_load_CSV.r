# Purpose   : function to load data.frame from a CSV file. 
# Input     : name of new variable, format settings:
#             ";" delimiter,
#             "." for decimal,
#             "NA" for missing values,
#             "" for string
#             header : 1 line with field name
#             file name and path trough a "input box"
# Output    : none directly; a variable tabella with imported data is made in
#             Global enviromental
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.5
# Licence   : GPL2.1
#
require(tools)
require(stringr)
nome_file<-file.choose(new = FALSE)
repeat{
  ans<-inpboxeeeeeekk(c('* Matrix Name','* Field Separator','* Decimal Separator','* Missing Data','* Skip Top Rows','* Skip Left Columns'),
                      c('Header','Row Names'),
                      c(str_replace_all(file_path_sans_ext(basename(nome_file))," ",""),';','.','NA','0','0','TRUE','FALSE'))
  if(is.null(ans))break
  if(!is.null(ans)){
    variable<-NULL
    variable$input<-ans[[1]]
    if(variable$input==make.names(variable$input)){
      oyn<-'yes'
      if(exists(variable$input,envir=.GlobalEnv)){
        oyn<-tk_messageBox(type="yesno",message='Variable Exists. Overwrite ?')
      }
      if(oyn=='yes'){
        if(length(variable$input)!=0){
          variable$value<-read.csv2(nome_file,header=as.logical(ans[[7]]),sep=ans[[2]],
                                    quote="\"",dec=ans[[3]],na.strings=ans[[4]],stringsAsFactors=FALSE,blank.lines.skip=TRUE)
          if(as.numeric(ans[[5]])!=0)variable$value<-variable$value[-(1:as.numeric(ans[[5]])),]
          if(as.numeric(ans[[6]])!=0)variable$value<-variable$value[,-(1:as.numeric(ans[[6]]))]
          if(as.logical(ans[[8]])){
            rownames(variable$value)<-variable$value[,1]
            variable$value<-variable$value[,-1]
          }
          variable$existance<-'TRUE'
          variable$control<-1
          variable$name<-variable$input
        }
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



