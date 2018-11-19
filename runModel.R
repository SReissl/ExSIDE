source("Model.R")
source("Initialise.R")
initreturn<-init()
inits<-initreturn$init
params<-initreturn$param
bankinits<-initreturn$bankinit
runModel(params=params,inits=inits,bankinits=bankinits)


