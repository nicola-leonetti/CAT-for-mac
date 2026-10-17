# Purpose   : bar plot of the predictions of a single object after randomized CV
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: CM
# Version   : 2.3
# Licence   : GPL2.1

ans<-inpboxc(tcombo = "row name",vcombo = 
rownames(CLA_RND$om),inp = -1)
if(!is.null(ans)){
  dev.new(title=paste("bar plot predictions object",rownames(CLA_RND$om)[ans[[1]]]))
  x<-factor(CLA_RND$om[rownames(CLA_RND$om)==rownames(CLA_RND$om)[ans[[1]]],],levels=CLA_RND$lev)
  barplot(main=paste("predictions object",rownames(CLA_RND$om)[ans[[1]]]),table(x)/ncol(CLA_RND$om)*100,ylim=c(0,100))
  rm(x)
}



