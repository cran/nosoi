## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setupA, eval=FALSE-------------------------------------------------------
#  SimulationSingle <- nosoiSim(type="single", popStructure="none", ...)

## ----setupB, eval=FALSE-------------------------------------------------------
#  SimulationSingle <- nosoiSim(type="single", popStructure="none",
#                               length.sim=300, max.infected=1000, init.individuals=1, ...)

## ----setupA-dual, eval=FALSE--------------------------------------------------
#  SimulationDual <- nosoiSim(type="dual", popStructure="none", ...)

## ----pExit1, eval=FALSE-------------------------------------------------------
#  p_Exit_fct  <- function(t){return(0.08)}

## ----nContact1, eval=FALSE----------------------------------------------------
#  n_contact_fct = function(t){abs(round(rnorm(1, 0.5, 1), 0))}

## ----nContact2, echo=FALSE----------------------------------------------------
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Package 'ggplot2' is needed for plotting this figure.")
} else {
  library(ggplot2)
  library(dplyr)
  set.seed(4099)
  data = data.frame(N=abs(round(rnorm(200, 0.5, 1), 0)))
  
  data = data %>% group_by(N) %>% summarise(freq=length(N)/200)
  
  ggplot(data=data, aes(x=as.factor(N), y=freq)) + geom_bar(stat="identity") + theme_minimal() + labs(x="nContact",y="Frequency")
}

## ----pTrans1, eval=FALSE------------------------------------------------------
#  p_Trans_fct <- function(t, p_max, t_incub){
#      if(t < t_incub){p=0}
#      if(t >= t_incub){p=p_max}
#      return(p)
#  }

## ----pTrans2, eval=FALSE------------------------------------------------------
#  t_incub_fct <- function(x){rnorm(x,mean = 7,sd=1)}
#  p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}

## ----pTrans3, echo=FALSE------------------------------------------------------
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Package 'ggplot2' is needed for plotting this figure.")
} else {
  library(ggplot2)
  library(dplyr)
  set.seed(99)
  
  p_Trans_fct <- function(t, p_max, t_incub){
    if(t < t_incub){p=0}
    if(t >= t_incub){p=p_max}
    return(p)
  }
  
  t_incub_fct <- function(x){rnorm(x,mean = 7,sd=1)}
  p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}
  
  data = data.frame(t_incub=t_incub_fct(200),p_max=p_max_fct(200),host=paste0("H-",1:200))
  
  t=c(0:12)
  data3=NULL
  for(t in 0:15){
    data2 = data %>% group_by(host) %>% mutate(proba=p_Trans_fct(t=t,p_max=p_max, t_incub=t_incub))
    data2$t = t
    data3 = rbind(data3, data2)
  }
  
  ggplot(data=data3, aes(x=t, y=proba,group=host)) + geom_line(color="grey60") + theme_minimal() + labs(x="Time since infection (t)",y="pTrans")
}

## ----pTrans4, eval=FALSE------------------------------------------------------
#  t_incub_fct <- function(x){rnorm(x,mean = 7,sd=1)}
#  p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}
#  
#  param_pTrans = list(p_max=p_max_fct, t_incub=t_incub_fct)

## ----setupF-------------------------------------------------------------------
library(nosoi)  

#pExit
p_Exit_fct  <- function(t){return(0.08)}

#nContact
n_contact_fct = function(t){abs(round(rnorm(1, 0.5, 1), 0))}

#pTrans
p_Trans_fct <- function(t,p_max,t_incub){
    if(t < t_incub){p=0}
    if(t >= t_incub){p=p_max}
    return(p)
}
 
t_incub_fct <- function(x){rnorm(x,mean = 7,sd=1)}
p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}

param_pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct)

# Starting the simulation ------------------------------------

set.seed(805)
SimulationSingle <- nosoiSim(type="single", popStructure="none",
                             length.sim=100, max.infected=100, init.individuals=1, 
                             nContact=n_contact_fct,
                             param.nContact=NA,
                             timeDep.nContact=FALSE,
                             pExit = p_Exit_fct,
                             param.pExit=NA,
                             timeDep.pExit=FALSE,
                             pTrans = p_Trans_fct,
                             param.pTrans = param_pTrans,
                             timeDep.pTrans=FALSE,
                             prefix.host="H",
                             print.progress=FALSE)

## ----pExit1-dual, eval=FALSE--------------------------------------------------
#  p_Exit_fctB  <- function(t,prestime){(sin(prestime/(2*pi*10))+1)/16} #for a periodic function

## ----pExit2-dual, echo=FALSE--------------------------------------------------
p_Exit_fctx <- function(x){(sin(x/(2*pi*10))+1)/16} #for a periodic function
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Package 'ggplot2' is needed for plotting this figure.")
} else {
  ggplot(data=data.frame(x=0), aes(x=x)) + stat_function(fun=p_Exit_fctx) + theme_minimal() + labs(x="Absolute time (prestime)",y="pExit") + xlim(0,360)
}

## ----nContact1.B, eval=FALSE--------------------------------------------------
#  n_contact_fct.B = function(t){sample(c(0,1,2),1,prob=c(0.6,0.3,0.1))}

## ----nContact2.B, echo=FALSE--------------------------------------------------
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Package 'ggplot2' is needed for plotting this figure.")
} else {
  library(ggplot2)
  library(dplyr)
  set.seed(9898)
  data = data.frame(N=sample(c(0,1,2),200,replace=TRUE,prob=c(0.6,0.3,0.1)))
  
  data = data %>% group_by(N) %>% summarise(freq=length(N)/200)
  
  ggplot(data=data, aes(x=as.factor(N), y=freq)) + geom_bar(stat="identity") + theme_minimal() + labs(x="nContact.B",y="Frequency")
}

## ----pTrans1.B, eval=FALSE----------------------------------------------------
#  p_Trans_fct.B <- function(t, max.time){
#    dnorm(t, mean=max.time, sd=2)*5
#  }

## ----pTrans2.B, eval=FALSE----------------------------------------------------
#  max.time_fct <- function(x){rnorm(x,mean = 5,sd=1)}

## ----pTrans3.B, echo=FALSE----------------------------------------------------
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Package 'ggplot2' is needed for plotting this figure.")
} else {
  library(ggplot2)
  library(dplyr)
  set.seed(7979)
  
  p_Trans_fct <- function(t, max.time){
    dnorm(t, mean=max.time, sd=2)*5
  }
  
  max.time_fct <- function(x){rnorm(x,mean = 5,sd=1)}
  
  data = data.frame(max.time=max.time_fct(200),host=paste0("H-",1:200))
  
  t=c(0:12)
  data3=NULL
  for(t in 0:15){
    data2 = data %>% group_by(host) %>% mutate(proba=p_Trans_fct(t=t,max.time=max.time))
    data2$t = t
    data3 = rbind(data3, data2)
  }
  
  ggplot(data=data3, aes(x=t, y=proba,group=host)) + geom_line(color="grey60",alpha=0.3) + theme_minimal() + labs(x="Time since infection (t)",y="pTrans")
}

## ----pTrans4.B, eval=FALSE----------------------------------------------------
#  max.time_fct <- function(x){rnorm(x,mean = 5,sd=1)}
#  
#  param_pTrans.B = list(max.time=max.time_fct)

## ----setupF.B-----------------------------------------------------------------
library(nosoi)

#HostA ------------------------------------

#pExit
p_Exit_fct.A  <- function(t){return(0.08)}

#nContact
n_contact_fct.A = function(t){abs(round(rnorm(1, 0.5, 1), 0))}

#pTrans
p_Trans_fct.A <- function(t,p_max,t_incub){
    if(t < t_incub){p=0}
    if(t >= t_incub){p=p_max}
    return(p)
}
 
t_incub_fct <- function(x){rnorm(x,mean = 7,sd=1)}
p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}

param_pTrans.A = list(p_max=p_max_fct,t_incub=t_incub_fct)

#Host B ------------------------------------

#pExit
p_Exit_fct.B  <- function(t,prestime){(sin(prestime/(2*pi*10))+1)/16}

#nContact
n_contact_fct.B = function(t){sample(c(0,1,2),1,prob=c(0.6,0.3,0.1))}

#pTrans
p_Trans_fct.B <- function(t, max.time){
  dnorm(t, mean=max.time, sd=2)*5
}
 
max.time_fct <- function(x){rnorm(x,mean = 5,sd=1)}

param_pTrans.B = list(max.time=max.time_fct)

# Starting the simulation ------------------------------------

set.seed(606)
SimulationDual <- nosoiSim(type="dual", popStructure="none",
                           length.sim=100, 
                           max.infected.A=100, 
                           max.infected.B=100, 
                           
                           init.individuals.A=1, 
                           init.individuals.B=0, 
                           
                           nContact.A=n_contact_fct.A,
                           param.nContact.A=NA,
                           timeDep.nContact.A=FALSE,
                           pExit.A=p_Exit_fct.A,
                           param.pExit.A=NA,
                           timeDep.pExit.A=FALSE,
                           pTrans.A=p_Trans_fct.A,
                           param.pTrans.A=param_pTrans.A,
                           timeDep.pTrans.A=FALSE,
                           prefix.host.A="H",
                           
                           nContact.B=n_contact_fct.B,
                           param.nContact.B=NA,
                           timeDep.nContact.B=FALSE,
                           pExit.B=p_Exit_fct.B,
                           param.pExit.B=NA,
                           timeDep.pExit.B=TRUE,
                           pTrans.B=p_Trans_fct.B,
                           param.pTrans.B=param_pTrans.B,
                           timeDep.pTrans.B=FALSE,
                           prefix.host.B="V",
                           
                           print.progress=FALSE)

