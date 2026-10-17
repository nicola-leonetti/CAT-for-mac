# Purpose   : allow to load a new dataset in the exploratory tool
# Authors   : R. Leardi, C. Melzi, G. Polotti
# Programmer: GMP
# Version   : 1.0
# Licence   : GPL2.1
#
GOBI<-NULL
variable<-gvfib()
M<-variable$value
if((typeof(M)=='list')|(typeof(M)=='double')){GOBI<-ggobi(M)}else{
tk_messageBox(type=c("ok"),message='Only Matrix for this Function!',
caption="Input Error")}
