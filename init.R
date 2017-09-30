## First part, install required libraries
if( !require( randomForest ) ) install.packages("randomForest");require(randomForest)
if( !require( parallel ) ) install.packages("parallel");require(parallel)
if( !require( plyr ) ) install.packages("plyr");require(plyr)
if( !require( gatbxr ) ){
  if( !require( devtools ) ) install.packages("devtools")
  devtools::install_github('drizztxx/gatbxr')
  require(gatbxr)
}
## Second part, initialized mpgarf environment
if('.mpgarf' %in% search()) detach('.mpgarf')
if(exists('.mpgarf')) rm(.mpgarf)
if(exists('.tempVars')) rm(.tempVars)
.mpgarf = new.env()
.tempVars = new.env()

assign('pathIntrospection', function(){
  frameFiles = Filter(Negate(is.null),
                      lapply(sys.frames(),function(x)x$ofile))
  return(dirname(frameFiles[[length(frameFiles)]]))},
  envir = .tempVars
)
.tempVars$loadpath = .tempVars$pathIntrospection()

.tempVars$rfiles = list.files(.tempVars$loadpath, pattern = "[^init][.][Rr]$")
.tempVars$path2rfiles = sapply(.tempVars$rfiles,function(x){
  ifelse(.tempVars$loadpath!='',file.path(.tempVars$loadpath,x),x)})

sapply(.tempVars$path2rfiles,function(x)sys.source(x,.mpgarf))
attach(.mpgarf,warn.conflicts=FALSE)
rm(.tempVars)

