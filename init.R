require(randomForest)
require(parallel)
require(plyr)
require(R.matlab)
# Matlab$startServer(host="192.168.91.66",port=9997)
# matlab <- Matlab(host="192.168.91.66",port=9997)
Matlab$startServer(port=9998)
matlab <- Matlab(port=9998)
isOpen <- open(matlab)
if (!isOpen) stop("Matlab Server Cannot Conect Correctly")

if('.mpgarf' %in% search()) detach('.mpgarf')
if(exists('.mpgarf')) rm(.mpgarf)
if(exists('.tempVars')) rm(.tempVars)
.mpgarf = new.env()
.tempVars = new.env()
####
assign('pathIntrospection', function(){
  frameFiles = Filter(Negate(is.null),
                      lapply(sys.frames(),function(x)x$ofile))
  return(dirname(frameFiles[[length(frameFiles)]]))},
  envir = .tempVars
)
.tempVars$loadpath = .tempVars$pathIntrospection()
####
.tempVars$rfiles = list.files(.tempVars$loadpath, pattern = "[^init][.][Rr]$")
.tempVars$path2rfiles = sapply(.tempVars$rfiles,function(x){
  ifelse(.tempVars$loadpath!='',file.path(.tempVars$loadpath,x),x)})

sapply(.tempVars$path2rfiles,function(x)sys.source(x,.mpgarf))
# compiled version of functions
# .tempVars$funname = sub("[.][Rr]$","",.tempVars$rfiles)
# m_ply(.tempVars$funname,function(x)assign(x,value = cmpfun(get(x,envir = .garf)),envir= .garf))
attach(.mpgarf,warn.conflicts=FALSE)
rm(.tempVars)

