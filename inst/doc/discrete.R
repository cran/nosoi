## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setupMatrix2, echo=FALSE-------------------------------------------------
matrix(c(0, 0.2, 0.4, 
         0.5, 0, 0.6, 
         0.5, 0.8, 0),
       nrow = 3, ncol = 3,
       dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

## ----setupMatrix, echo=FALSE, message=FALSE,warning=FALSE---------------------
if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("viridis", quietly = TRUE) || !requireNamespace("igraph", quietly = TRUE) || !requireNamespace("ggnetwork", quietly = TRUE)) {
  message("Packages 'ggplot2', 'viridis', 'igraph' and 'ggnetwork'  are needed for plotting this figure.")
} else {
  library(ggplot2)
  library(viridis)
  library(igraph)
  library(ggnetwork)
  
  transition.matrix <- matrix(c(0,0.2,0.4,0.5,0,0.6,0.5,0.8,0), nrow = 3, ncol = 3, dimnames=list(c("A","B","C"),c("A","B","C")))
  
  melted.transition.matrix <- reshape2::melt(transition.matrix, varnames = c("from","to"), value.name="prob", as.is = TRUE) #melting the matrix go get from -> to in one line with probability
  
  melted.transition.matrix <- subset(melted.transition.matrix, prob!=0)
  
  graph.Matrix <- igraph::graph.data.frame(melted.transition.matrix,directed=T)
  
graph.Matrix <- igraph::graph.data.frame(melted.transition.matrix,directed=T)
  graph.Matrix2 = igraph::layout_in_circle(graph.Matrix)
  
  graph.matrix.network <- ggnetwork::ggnetwork(graph.Matrix, layout = graph.Matrix2, arrow.gap=0.18) #using ggnetwork to provide the layout
  
  #plotting the network
  ggplot(graph.matrix.network, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = "grey70", arrow = arrow(length = unit(1, "lines"), type = "closed"), curvature = 0.2) + geom_nodes(aes(color = name) , size = 30) +
    geom_nodetext(aes(label = name), color="white", fontface = "bold",size=15) + scale_color_viridis(guide=FALSE, discrete=TRUE) +
    theme_blank() + ylim(-0.5,1.2) + xlim(-0.5,1.2) +
    geom_text(data=graph.matrix.network,aes(x=(x+xend)/2,y=(y+yend)/2,label = prob,color=name), size = 6)
}

## ----setupA, eval=FALSE-------------------------------------------------------
#  SimulationSingle <- nosoiSim(type="single", popStructure="discrete", ...)

## ----setupB, eval=FALSE-------------------------------------------------------
#  SimulationSingle <- nosoiSim(type="single", popStructure="none",
#                               length.sim=300, max.infected=1000, init.individuals=1, ...)

## ----setupA-dual, eval=FALSE--------------------------------------------------
#  SimulationDual <- nosoiSim(type="dual", popStructure="discrete", ...)

## ----pExit1, eval=FALSE-------------------------------------------------------
#  p_Exit_fct  <- function(t,current.in){
#    if(current.in=="A"){return(0.02)}
#    if(current.in=="B"){return(0.05)}
#    if(current.in=="C"){return(0.1)}}
#  }

## ----pMove1, eval=FALSE-------------------------------------------------------
#  p_Move_fct  <- function(t){return(0.1)}

## ----nContact1, eval=FALSE----------------------------------------------------
#  n_contact_fct = function(t){abs(round(rnorm(1, 0.5, 1), 0))}

## ----nContact2, echo=FALSE, message=FALSE-------------------------------------
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Package 'ggplot2' is needed for plotting this figure.")
} else {
  library(ggplot2)
  library(dplyr)
  set.seed(900)
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
#  t_incub_fct <- function(x){rnorm(x, mean=7, sd=1)}
#  p_max_fct <- function(x){rbeta(x, shape1=5, shape2=2)}

## ----pTrans3, echo=FALSE------------------------------------------------------
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Package 'ggplot2' is needed for plotting this figure.")
} else {
  library(ggplot2)
  library(dplyr)
  
  set.seed(506)
  
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
#  param_pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct)

## ----setupF-------------------------------------------------------------------
library(nosoi)

#Transition matrix
transition.matrix <- matrix(c(0,0.2,0.4,0.5,0,0.6,0.5,0.8,0),nrow = 3, ncol = 3,dimnames=list(c("A","B","C"),c("A","B","C")))

#pExit
p_Exit_fct  <- function(t,current.in){
  if(current.in=="A"){return(0.02)}
  if(current.in=="B"){return(0.05)}
  if(current.in=="C"){return(0.1)}
}

#pMove
p_Move_fct  <- function(t){return(0.1)}

#nContact
n_contact_fct = function(t){abs(round(rnorm(1, 0.5, 1), 0))}

#pTrans
proba <- function(t,p_max,t_incub){
  if(t <= t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)
}

t_incub_fct <- function(x){rnorm(x,mean = 5,sd=1)}
p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}

param_pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct)

# Starting the simulation ------------------------------------

set.seed(846)
SimulationSingle <- nosoiSim(type="single", popStructure="discrete",
                             length.sim=300, max.infected=300, init.individuals=1, init.structure="A", 
                             
                             structure.matrix=transition.matrix,
                             
                             pExit = p_Exit_fct,
                             param.pExit=NA,
                             timeDep.pExit=FALSE,
                             diff.pExit=TRUE,
                             
                             pMove = p_Move_fct,
                             param.pMove=NA,
                             timeDep.pMove=FALSE,
                             diff.pMove=FALSE,
                             
                             nContact=n_contact_fct,
                             param.nContact=NA,
                             timeDep.nContact=FALSE,
                             diff.nContact=FALSE,
                             
                             pTrans = proba,
                             param.pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct),
                             timeDep.pTrans=FALSE,
                             diff.pTrans=FALSE,
                             
                             prefix.host="H",
                             print.progress=FALSE,
                             print.step=10)

## ----pExit1-dual, eval=FALSE--------------------------------------------------
#  p_Exit_fctB  <- function(t,prestime){(sin(prestime/(2*pi*10))+1)/16} #for a periodic function

## ----pExit2-dual, echo=FALSE--------------------------------------------------
p_Exit_fctx <- function(x){(sin(x/(2*pi*10))+1)/16} #for a periodic function
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Package 'ggplot2' is needed for plotting this figure.")
} else {
  ggplot(data=data.frame(x=0), aes(x=x)) + stat_function(fun=p_Exit_fctx) + theme_minimal() + labs(x="Absolute time (prestime)",y="pExit") + xlim(0,360)
}

## ----pMove.B, eval=FALSE------------------------------------------------------
#  p_Move_fct.B  <- NA

## ----nContact1.B, eval=FALSE--------------------------------------------------
#  n_contact_fct.B = function(t){sample(c(0,1,2),1,prob=c(0.6,0.3,0.1))}

## ----nContact2.B, echo=FALSE--------------------------------------------------
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Package 'ggplot2' is needed for plotting this figure.")
} else {
  library(ggplot2)
  library(dplyr)
  set.seed(6059)
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
  set.seed(6609)
  
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

#Transition matrix
transition.matrix <- matrix(c(0,0.2,0.4,0.5,0,0.6,0.5,0.8,0),nrow = 3, ncol = 3,dimnames=list(c("A","B","C"),c("A","B","C")))

#Host A -----------------------------------

#pExit
p_Exit_fct  <- function(t,current.in){
  if(current.in=="A"){return(0.02)}
  if(current.in=="B"){return(0.05)}
  if(current.in=="C"){return(0.1)}
}

#pMove
p_Move_fct  <- function(t){return(0.1)}

#nContact
n_contact_fct = function(t){abs(round(rnorm(1, 0.5, 1), 0))}

#pTrans
proba <- function(t,p_max,t_incub){
  if(t <= t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)
}

t_incub_fct <- function(x){rnorm(x,mean = 5,sd=1)}
p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}

param_pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct)

#Host B -----------------------------------

#pExit
p_Exit_fct.B  <- function(t,prestime){(sin(prestime/(2*pi*10))+1)/16}

#pMove
p_Move_fct.B  <- NA

#nContact
n_contact_fct.B = function(t){sample(c(0,1,2),1,prob=c(0.6,0.3,0.1))}

#pTrans
p_Trans_fct.B <- function(t, max.time){
  dnorm(t, mean=max.time, sd=2)*5
}
 
max.time_fct <- function(x){rnorm(x,mean = 5,sd=1)}

param_pTrans.B = list(max.time=max.time_fct)

# Starting the simulation ------------------------------------

set.seed(60)
SimulationDual <- nosoiSim(type="dual", popStructure="discrete",
                           length.sim=300, 
                           max.infected.A=100,
                           max.infected.B=200,
                           init.individuals.A=1,
                           init.individuals.B=0,
                           init.structure.A="A",
                           init.structure.B=NA,
                           structure.matrix.A=transition.matrix,
                           structure.matrix.B=transition.matrix,
                           
                           pExit.A = p_Exit_fct,
                           param.pExit.A=NA,
                           timeDep.pExit.A=FALSE,
                           diff.pExit.A=TRUE,
                           
                           pMove.A = p_Move_fct,
                           param.pMove.A=NA,
                           timeDep.pMove.A=FALSE,
                           diff.pMove.A=FALSE,
                           
                           nContact.A=n_contact_fct,
                           param.nContact.A=NA,
                           timeDep.nContact.A=FALSE,
                           diff.nContact.A=FALSE,
                           
                           pTrans.A = proba,
                           param.pTrans.A = list(p_max=p_max_fct,t_incub=t_incub_fct),
                           timeDep.pTrans.A=FALSE,
                           diff.pTrans.A=FALSE,
                           prefix.host.A="H",
                           
                           pExit.B = p_Exit_fct.B,
                           param.pExit.B=NA,
                           timeDep.pExit.B=TRUE,
                           diff.pExit.B=FALSE,
                           
                           pMove.B = p_Move_fct.B,
                           param.pMove.B=NA,
                           timeDep.pMove.B=FALSE,
                           diff.pMove.B=FALSE,
                           
                           nContact.B=n_contact_fct.B,
                           param.nContact.B=NA,
                           timeDep.nContact.B=FALSE,
                           diff.nContact.B=FALSE,
                           
                           pTrans.B = p_Trans_fct.B,
                           param.pTrans.B = param_pTrans.B,
                           timeDep.pTrans.B=FALSE,
                           diff.pTrans.B=FALSE,
                           prefix.host.B="V",
                           
                           print.progress=FALSE)

