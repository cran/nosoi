## ----setupMatrix, echo=FALSE, message=FALSE,warning=FALSE---------------------
if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("viridis", quietly = TRUE) || !requireNamespace("igraph", quietly = TRUE) || !requireNamespace("ggnetwork", quietly = TRUE)) {
  message("Packages 'ggplot2', 'viridis', 'igraph' and 'ggnetwork'  are needed for plotting this figure.")
} else {
  library(ggplot2)
  library(viridis)
  library(igraph)
  library(ggnetwork)
  
  transition.matrix = matrix(c(0,0,0,0,0,
                               1,0,0,0,0,
                               0,1,0,0,0,
                               0,0,1,0,0,
                               0,0,0,1,0),nrow = 5, ncol = 5,
                             dimnames=list(c("pExit","pMove","sdMove","nContact","pTrans"),
                                           c("pExit","pMove","sdMove","nContact","pTrans")))
  
  melted.transition.matrix <- reshape2::melt(transition.matrix, varnames = c("from","to"),value.name="prob", as.is = TRUE) #melting the matrix go get from -> to in one line with probability
  
  melted.transition.matrix <- subset(melted.transition.matrix, prob!=0)
  
  graph.Matrix <- igraph::graph.data.frame(melted.transition.matrix,directed=T)
  graph.Matrix2 = igraph::layout_in_circle(graph.Matrix)
  
  graph.simA.network <- ggnetwork::ggnetwork(graph.Matrix, layout = graph.Matrix2, arrow.gap=0.18) #using ggnetwork to provide the layout
  
  #plotting the network
  ggplot(graph.simA.network, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = "grey70",arrow = arrow(length = unit(1, "lines"), type = "closed"),curvature = 0.2) + geom_nodes(aes(color = name) , size = 30) +
    geom_nodetext(aes(label = name),color="white", fontface = "bold",size=3) + scale_color_viridis(guide=FALSE,discrete=TRUE) +
    theme_blank() + ylim(-0.5,1.2) + xlim(-0.5,1.2)
}

## ----pFunction1, eval = FALSE-------------------------------------------------
#  p_Function  <- function(t){0.08}

## ----pFunction2, eval = FALSE-------------------------------------------------
#  p_Function  <- function(t){plogis(t,10,2)}

## ----pFunction3, eval = FALSE-------------------------------------------------
#  p_Function  <- function(t,prestime){(sin(prestime/12)+1)/2}

## ----pFunction4, eval = FALSE-------------------------------------------------
#  p_Function  <- function(t,current.in){
#    if(current.in=="A"){return(0)}
#    if(current.in=="B"){return(0.5)}
#    if(current.in=="C"){return(1)}} #discrete (between states "A","B" and "C")
#  
#  p_Function  <- function(t,current.env.value){current.env.value/100} #continuous

## ----pFunction4.2, eval = FALSE-----------------------------------------------
#  p_Function  <- function(t,current.in, host.count){
#    if(current.in=="A"){return(0)}
#    if(current.in=="B" & host.count < 300 ){return(0.5)}
#    if(current.in=="B" & host.count >= 300 ){return(0)}
#    if(current.in=="C"){return(1)}} #discrete (between states "A","B" and "C")
#  
#  p_Function  <- function(t,current.env.value,host.count){(current.env.value-host.count)/100} #continuous

## ----pFunction5, eval = FALSE-------------------------------------------------
#  p_Function  <- function(t,pFunction.param1){plogis(t,pFunction.param1,2)}

## ----pFunction6, eval = FALSE-------------------------------------------------
#  p_Function_param1 <- function(x){rnorm(x,mean=10,sd=2)} #sampling one parameter for each infected individual

## ----pFunction7, eval = FALSE-------------------------------------------------
#  p_Function_parameters  <- list(pFunction.param1 = p_Function_param1)

## ----pFunction8, eval = FALSE-------------------------------------------------
#  p_Function  <- function(t, prestime, current.in, pFunction.param1, pFunction.param2,...){}

