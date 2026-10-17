# Purpose   : 
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.1
# Licence   : GPL2.1
#
if(!exists('repl_r.set'))repl_r.set<-c(previous.name,'','','')
ans<-inpboxeee('*Matrix name','*First column','*Last column',vinp=repl_r.set)
if(!is.null(ans)){
  previous.name<-ans[[1]]
  repl_r.set<-ans
  var.trans<-eval(parse(text=ans[[1]]),envir=.GlobalEnv)
  M<-var.trans[,ans[[2]]:ans[[3]],drop=FALSE]
  M<-as.data.frame(M)
  
  # Identify groups of replicate rows
  group_id <- interaction(M, drop = TRUE)
  groups <- split(seq_len(nrow(M)), group_id)
  duplicate_groups <- groups[sapply(groups, length) > 1]
  
  # Print replicate row groups
  for (g in duplicate_groups) {
    cat("Replicate rows:", g, "\n")
  }
  
  rm(M,var.trans,group_id,groups,duplicate_groups,g)
}