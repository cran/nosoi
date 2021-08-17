## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----getHostData, eval = FALSE------------------------------------------------
#  getHostData(nosoi.output, what, pop)

## ----getTableHosts, eval = FALSE----------------------------------------------
#  getTableHosts(nosoi.output, pop)

## ----getTableState, eval = FALSE----------------------------------------------
#  getTableState(nosoi.output, pop)

## ----nosoiSummary, eval = FALSE-----------------------------------------------
#  summary(nosoi.output)

## ----simDynamics, message=FALSE-----------------------------------------------
library(nosoi)
  t_incub_fct <- function(x){rnorm(x,mean = 5,sd=1)}
  p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}
  p_Move_fct  <- function(t){return(0.1)}

  p_Exit_fct  <- function(t){return(0.05)}

  proba <- function(t,p_max,t_incub){
    if(t <= t_incub){p=0}
    if(t >= t_incub){p=p_max}
    return(p)
  }

  time_contact <- function(t, current.in, host.count){

    temp.val = 30 - host.count

    if(temp.val <= 0) {
      return(0)
    }
    if(temp.val >= 0) {
      if(current.in=="A"){
        return(round((temp.val/30)*rnorm(1, 3, 1), 0))}
      if(current.in=="B"){return(0)}
      if(current.in=="C"){
        return(round((temp.val/30)*rnorm(1, 6, 1), 0))}
    }
  }

  transition.matrix = matrix(c(0,0.2,0.4,0.5,0,0.6,0.5,0.8,0),nrow = 3, ncol = 3,dimnames=list(c("A","B","C"),c("A","B","C")))

  set.seed(1050)
  test.nosoiA <- nosoiSim(type="single", popStructure="discrete",
                          length=100,
                          max.infected=200,
                          init.individuals=1,
                          init.structure="A",
                          structure.matrix=transition.matrix,
                          pMove=p_Move_fct,
                          param.pMove=NA,
                          diff.nContact=TRUE,
                          hostCount.nContact=TRUE,
                          nContact=time_contact,
                          param.nContact=NA,
                          pTrans = proba,
                          param.pTrans = list(p_max=p_max_fct,
                                              t_incub=t_incub_fct),
                          pExit=p_Exit_fct,
                          param.pExit=NA
  )

## ----figureDynamics1, eval=FALSE, message=FALSE-------------------------------
#  library(ggplot2)
#  cumulative.table <- getCumulative(test.nosoiA)
#  dynamics.table <- getDynamic(test.nosoiA)
#  
#  ggplot(data=cumulative.table, aes(x=t, y=Count)) + geom_line() + theme_minimal() + labs(x="Time (t)",y="Cumulative count of infected hosts")

## ----figureDynamics1_actual, echo=FALSE, message=FALSE------------------------
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Package 'ggplot2' is needed for plotting this figure.")
} else {
  library(ggplot2)
  cumulative.table <- getCumulative(test.nosoiA)
  dynamics.table <- getDynamic(test.nosoiA)
  
  ggplot(data=cumulative.table, aes(x=t, y=Count)) + geom_line() + theme_minimal() + labs(x="Time (t)",y="Cumulative count of infected hosts")
}

## ----figureDynamics2, eval=FALSE, message=FALSE-------------------------------
#  ggplot(data=dynamics.table, aes(x=t, y=Count, color=state)) + geom_line() + theme_minimal() + labs(x="Time (t)",y="Number of active infected hosts")

## ----figureDynamics2_actual, echo=FALSE, message=FALSE------------------------
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Package 'ggplot2' is needed for plotting this figure.")
} else {
  ggplot(data=dynamics.table, aes(x=t, y=Count, color=state)) + geom_line() + theme_minimal() + labs(x="Time (t)",y="Number of active infected hosts")
}

## ----simDynamics-bis, eval=FALSE, message=FALSE-------------------------------
#  library(nosoi)
#    t_incub_fct <- function(x){rnorm(x,mean = 5,sd=1)}
#    p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}
#    p_Move_fct  <- function(t){return(0.1)}
#  
#    p_Exit_fct  <- function(t){return(0.05)}
#  
#    proba <- function(t,p_max,t_incub){
#      if(t <= t_incub){p=0}
#      if(t >= t_incub){p=p_max}
#      return(p)
#    }
#  
#    time_contact <- function(t, current.in, host.count){
#  
#      temp.val = 30 - host.count
#  
#      if(temp.val <= 0) {
#        return(0)
#      }
#      if(temp.val >= 0) {
#        if(current.in=="A"){
#          return(round((temp.val/30)*rnorm(1, 3, 1), 0))}
#        if(current.in=="B"){return(0)}
#        if(current.in=="C"){
#          return(round((temp.val/30)*rnorm(1, 6, 1), 0))}
#      }
#    }
#  
#    transition.matrix = matrix(c(0,0.2,0.4,0.5,0,0.6,0.5,0.8,0),nrow = 3, ncol = 3,dimnames=list(c("A","B","C"),c("A","B","C")))
#  
#    set.seed(1050)
#    test.nosoiA <- nosoiSim(type="single", popStructure="discrete",
#                            length=100,
#                            max.infected=200,
#                            init.individuals=1,
#                            init.structure="A",
#                            structure.matrix=transition.matrix,
#                            pMove=p_Move_fct,
#                            param.pMove=NA,
#                            diff.nContact=TRUE,
#                            hostCount.nContact=TRUE,
#                            nContact=time_contact,
#                            param.nContact=NA,
#                            pTrans = proba,
#                            param.pTrans = list(p_max=p_max_fct,
#                                                t_incub=t_incub_fct),
#                            pExit=p_Exit_fct,
#                            param.pExit=NA
#    )

## ----R0-1, message=FALSE------------------------------------------------------
getR0(test.nosoiA)

## ----figureR0, eval=FALSE, message=FALSE--------------------------------------
#  data = data.frame(R0=getR0(test.nosoiA)$R0.dist)
#  ggplot(data=data, aes(x=R0)) + geom_histogram() + theme_minimal()

## ----figureR0_actual, echo=FALSE, message=FALSE-------------------------------
data = data.frame(R0=getR0(test.nosoiA)$R0.dist)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Package 'ggplot2' is needed for plotting this figure.")
} else {
  ggplot(data=data, aes(x=R0)) + geom_histogram() + theme_minimal()
}

## ----simDynamics-ter, eval=FALSE, message=FALSE-------------------------------
#  library(nosoi)
#  t_incub_fct <- function(x){rnorm(x,mean = 5,sd=1)}
#  p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}
#  p_Move_fct  <- function(t){return(0.1)}
#  
#  p_Exit_fct  <- function(t){return(0.05)}
#  
#  proba <- function(t,p_max,t_incub){
#    if(t <= t_incub){p=0}
#    if(t >= t_incub){p=p_max}
#    return(p)
#  }
#  
#  time_contact <- function(t, current.in, host.count){
#  
#    temp.val = 30 - host.count
#  
#    if(temp.val <= 0) {
#      return(0)
#    }
#    if(temp.val >= 0) {
#      if(current.in=="A"){
#        return(round((temp.val/30)*rnorm(1, 3, 1), 0))}
#      if(current.in=="B"){return(0)}
#      if(current.in=="C"){
#        return(round((temp.val/30)*rnorm(1, 6, 1), 0))}
#    }
#  }
#  
#  transition.matrix <- matrix(c(0,0.2,0.4,0.5,0,0.6,0.5,0.8,0),nrow = 3, ncol = 3,dimnames=list(c("A","B","C"),c("A","B","C")))
#  
#  set.seed(1050)
#  test.nosoiA <- nosoiSim(type="single", popStructure="discrete",
#                          length=100,
#                          max.infected=200,
#                          init.individuals=1,
#                          init.structure="A",
#                          structure.matrix=transition.matrix,
#                          pMove=p_Move_fct,
#                          param.pMove=NA,
#                          diff.nContact=TRUE,
#                          hostCount.nContact=TRUE,
#                          nContact=time_contact,
#                          param.nContact=NA,
#                          pTrans = proba,
#                          param.pTrans = list(p_max=p_max_fct,
#                                              t_incub=t_incub_fct),
#                          pExit=p_Exit_fct,
#                          param.pExit=NA
#  )

## ----treeA, eval=FALSE, message=FALSE-----------------------------------------
#  test.nosoiA.tree <- getTransmissionTree(test.nosoiA)
#  
#  library(ggplot2)
#  library(ggtree)
#  ggtree(test.nosoiA.tree, color = "gray30") + geom_nodepoint(aes(color=state)) + geom_tippoint(aes(color=state)) +
#    theme_tree2() + xlab("Time (t)") + theme(legend.position = c(0,0.8),
#                                             legend.title = element_blank(),
#                                             legend.key = element_blank())

## ----treeA_actual, echo=FALSE, message=FALSE----------------------------------
if (!requireNamespace("ape", quietly = TRUE) || !requireNamespace("tidytree", quietly = TRUE) || !requireNamespace("treeio", quietly = TRUE)) {
  message("Packages 'ape', 'tidytree' and 'treeio' are needed for transmission tree generation.")
} else {
  test.nosoiA.tree <- getTransmissionTree(test.nosoiA)
}

if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("ggtree", quietly = TRUE) || (utils::packageVersion("ggtree") < "3.1.3")) {
  message("Packages 'ggplot2' and 'ggtree (> 3.1.3)' are needed for plotting this figure.")
} else {
  library(ggplot2)
  library(ggtree)
  ggtree(test.nosoiA.tree, color = "gray30") + geom_nodepoint(aes(color=state)) + geom_tippoint(aes(color=state)) + 
    theme_tree2() + xlab("Time (t)") + theme(legend.position = c(0,0.8), 
                                             legend.title = element_blank(),
                                             legend.key = element_blank()) 
}

## ----tree-sample1, echo=FALSE, message=FALSE----------------------------------
if (!requireNamespace("ape", quietly = TRUE) || !requireNamespace("tidytree", quietly = TRUE) || !requireNamespace("treeio", quietly = TRUE)) {
  message("Packages 'ape', 'tidytree' and 'treeio' are needed for transmission tree generation.")
} else {
  library(dplyr)
  
  table.hosts.testA <- getTableHosts(test.nosoiA)
  setkey(table.hosts.testA, "hosts.ID")
  
  set.seed(30082019)
  sampled.hosts <- sample(table.hosts.testA$hosts.ID, 20)
  
  samples.data.table <- table.hosts.testA[sampled.hosts] %>% group_by(hosts.ID) %>% mutate(Sampled.time = round(runif(1, min=inf.time,max=out.time), digits = 0), labels = paste(hosts.ID,"sampled",sep="-")) 
  
  samples.data.table <- samples.data.table[c("hosts.ID","Sampled.time","labels")]
  colnames(samples.data.table) <- c("hosts", "times", "labels")
  samples.data.table
}

## ----tree-sample2, eval=FALSE, message=FALSE----------------------------------
#  test.nosoiA.tree.sampled <- sampleTransmissionTree(test.nosoiA, test.nosoiA.tree, samples.data.table)

## ----tree-sample2_actual, echo=FALSE, message=FALSE---------------------------
if (!requireNamespace("ape", quietly = TRUE) || !requireNamespace("tidytree", quietly = TRUE) || !requireNamespace("treeio", quietly = TRUE)) {
  message("Packages 'ape', 'tidytree' and 'treeio' are needed for transmission tree generation.")
} else {
  test.nosoiA.tree.sampled <- sampleTransmissionTree(test.nosoiA, test.nosoiA.tree, samples.data.table)
}

## ----treeB, eval = FALSE, message=FALSE---------------------------------------
#  ggtree(test.nosoiA.tree.sampled, color = "gray30") + geom_nodepoint(aes(color=state)) + geom_tippoint(aes(color=state)) + geom_tiplab(aes(label=host)) +
#    theme_tree2() + xlab("Time (t)") + theme(legend.position = c(0,0.8),
#                                             legend.title = element_blank(),
#                                             legend.key = element_blank())

## ----treeB_actual, echo=FALSE, message=FALSE----------------------------------
if (!requireNamespace("ggtree", quietly = TRUE) || (utils::packageVersion("ggtree") < "3.1.3")) {
  message("Package 'ggtree' is needed for plotting this figure.")
} else {
  ggtree(test.nosoiA.tree.sampled, color = "gray30") + geom_nodepoint(aes(color=state)) + geom_tippoint(aes(color=state)) + geom_tiplab(aes(label=host)) + 
    theme_tree2() + xlab("Time (t)") + theme(legend.position = c(0,0.8), 
                                             legend.title = element_blank(),
                                             legend.key = element_blank()) 
}

## ----tree-sample3, echo=FALSE, message=FALSE----------------------------------
if (!requireNamespace("ape", quietly = TRUE) || !requireNamespace("tidytree", quietly = TRUE) || !requireNamespace("treeio", quietly = TRUE)) {
  message("Packages 'ape', 'tidytree' and 'treeio' are needed for transmission tree generation.")
} else {
  set.seed(5905950)
  sampled.hosts2 = subset(table.hosts.testA, active == 0)$hosts.ID
  sampled.hosts <- sample(sampled.hosts2, 20)
  sampled.hosts
}

## ----tree-sample4, eval=FALSE, message=FALSE----------------------------------
#  test.nosoiA.tree.sampled.exiting <- sampleTransmissionTreeFromExiting(test.nosoiA.tree, sampled.hosts)

## ----tree-sample4_actual, echo=FALSE, message=FALSE---------------------------
if (!requireNamespace("ape", quietly = TRUE) || !requireNamespace("tidytree", quietly = TRUE) || !requireNamespace("treeio", quietly = TRUE)) {
  message("Packages 'ape', 'tidytree' and 'treeio' are needed for transmission tree generation.")
} else {
  test.nosoiA.tree.sampled.exiting <- sampleTransmissionTreeFromExiting(test.nosoiA.tree, sampled.hosts)
}

## ----treeC, eval=FALSE, message=FALSE-----------------------------------------
#  ggtree(test.nosoiA.tree.sampled.exiting, color = "gray30") + geom_nodepoint(aes(color=state)) + geom_tippoint(aes(color=state)) + geom_tiplab(aes(label=host)) +
#    theme_tree2() + xlab("Time (t)") + theme(legend.position = c(0,0.8),
#                                             legend.title = element_blank(),
#                                             legend.key = element_blank())

## ----treeC_actual, echo=FALSE, message=FALSE----------------------------------
if (!requireNamespace("ape", quietly = TRUE) || !requireNamespace("tidytree", quietly = TRUE) || !requireNamespace("treeio", quietly = TRUE) || !requireNamespace("ggtree", quietly = TRUE) || (utils::packageVersion("ggtree") < "3.1.3")) {
  message("Packages 'ape', 'tidytree', 'treeio' and 'ggtree' are needed for transmission tree generation.")
} else {
  ggtree(test.nosoiA.tree.sampled.exiting, color = "gray30") + geom_nodepoint(aes(color=state)) + geom_tippoint(aes(color=state)) + geom_tiplab(aes(label=host)) + 
    theme_tree2() + xlab("Time (t)") + theme(legend.position = c(0,0.8), 
                                             legend.title = element_blank(),
                                             legend.key = element_blank()) 
}

## ----writeTree, eval=FALSE, message=FALSE-------------------------------------
#  treeio::write.beast(test.nosoiA.tree.sampled.exiting)

