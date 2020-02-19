## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----RasterCode, eval = FALSE-------------------------------------------------
#  library(raster)
#  set.seed(860)
#  test.raster <- raster(nrows=100, ncols=100, xmn=-50, xmx=50, ymn=-50,ymx=50)
#  test.raster[] <- runif(10000, -80, 150)
#  test.raster <- focal(focal(test.raster, w=matrix(1, 5, 5), mean), w=matrix(1, 5, 5), mean)

## ----RasterCode2, echo = FALSE, message=FALSE---------------------------------
library(raster)
set.seed(860)
test.raster <- raster(nrows=100, ncols=100, xmn=-50, xmx=50, ymn=-50,ymx=50)
test.raster[] <- runif(10000, -80, 150)
test.raster <- focal(focal(test.raster, w=matrix(1, 5, 5), mean), w=matrix(1, 5, 5), mean)

test.raster_df <- as.data.frame(test.raster, xy = TRUE) 

if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("viridis", quietly = TRUE)) {
  message("Packages 'ggplot2' and 'viridis' are needed for plotting this figure.")
} else {
  library(ggplot2)
  library(viridis)
  
  ggplot() +
    geom_raster(data=test.raster_df, aes(x=x,y=y,fill=layer)) +
    scale_fill_viridis(limits=c(0,NA),na.value="white", name="Environmental\nvalue") +
    labs(caption = "Environmental raster") + theme_minimal() + coord_quickmap()
}

## ----RW-1, echo=FALSE, message = FALSE----------------------------------------
library(data.table)
library(raster)
x = seq(0,100,len=100)
y = seq(0,100,len=100)
a=1;b=2
r12 = raster(outer(x,y,function(x,y){a*x+b*y}))

test.raster_df <- as.data.frame(r12, xy = TRUE) 
max.raster=max(r12[], na.rm=T)

set.seed(95895)

move.test = data.table(t=0,current.in.x=0.25,current.in.y=0.75)
sdMove.values = 0.01

moveRotateContinuous.example = function(pt1, dX, dY, angle)
{
  s = sin(angle); c = cos(angle)
  x = dX; y = dY
  x_new = (x*c)-(y*s); y_new = (x*s)+(y*c)
  x_new = x_new+pt1[1]; y_new = y_new+pt1[2]
  return(c(x_new,y_new))
}


for (t in 2:1001){
  real.t = t-1
  
  current.move.pos.x = move.test[real.t,"current.in.x"]
  current.move.pos.y = move.test[real.t,"current.in.y"]
  
  current.sdMove.value = as.numeric(sdMove.values)
  
  positionFound1 = FALSE
  while (positionFound1 == FALSE)
  {
    counter = 0
    dX = rnorm(1, 0, current.sdMove.value)
    dY = rnorm(1, 0, current.sdMove.value)
    positionFound2 = FALSE
    while (positionFound2 == FALSE)
    {
      angle = (2*base::pi)*runif(1)
      newP = moveRotateContinuous.example(c(as.numeric(current.move.pos.x),as.numeric(current.move.pos.y)), dX, dY,angle)
      
      temp.env.value = raster::extract(r12,cbind(newP[1],newP[2]))
      
      if (!is.na(temp.env.value)){
        
        if (TRUE) {
          counter = counter+1
          v2 = temp.env.value/max.raster
          if (runif(1,0,1) < v2)
          {
            move.temp= data.table(t=real.t,current.in.x=newP[1],current.in.y=newP[2])
            
            positionFound2 = TRUE
            positionFound1 = TRUE
            if (counter == 30) {
              positionFound1 = TRUE
              positionFound2 = TRUE
            }
          }
        }
      }
    }
  }
  move.temp= data.table(t=real.t,current.in.x=newP[1],current.in.y=newP[2])
  move.test = rbindlist(list(move.test,move.temp))
}
move.test$type = "Attracted by raster"

move.test2 = data.table(t=0,current.in.x=0.25,current.in.y=0.75)
for (t in 2:1001){
  real.t = t-1
  
  current.move.pos.x = move.test2[real.t,"current.in.x"]
  current.move.pos.y = move.test2[real.t,"current.in.y"]
  
  current.sdMove.value = as.numeric(sdMove.values)
  
  positionFound1 = FALSE
  while (positionFound1 == FALSE)
  {
    counter = 0
    dX = rnorm(1, 0, current.sdMove.value)
    dY = rnorm(1, 0, current.sdMove.value)
    positionFound2 = FALSE
    while (positionFound2 == FALSE)
    {
      angle = (2*base::pi)*runif(1)
      newP = moveRotateContinuous.example(c(as.numeric(current.move.pos.x),as.numeric(current.move.pos.y)), dX, dY,angle)
      
      temp.env.value = raster::extract(r12,cbind(newP[1],newP[2]))
      
      if (!is.na(temp.env.value)){
        move.temp= data.table(t=real.t,current.in.x=newP[1],current.in.y=newP[2])
        positionFound1 = TRUE
        positionFound2 = TRUE
      }
    }
  }
  
  
  move.test2 = rbindlist(list(move.test2,move.temp))
  
}
move.test2$type = "Not attracted by raster"

move.test.all = rbindlist(list(move.test,move.test2))

if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("viridis", quietly = TRUE)) {
  message("Packages 'ggplot2' and 'viridis' are needed for plotting this figure.")
} else {
  ggplot() +
    geom_raster(data=test.raster_df, aes(x=x,y=y,fill=layer)) +
    # geom_line(data = move.test, mapping = aes(x = current.in.x, y = current.in.y), color = "grey90") +
    geom_point(data = move.test.all, aes(x = current.in.x, y = current.in.y,color=t)) +
    geom_point(data=data.table(t=0,current.in.x=0.25,current.in.y=0.75), aes(x = current.in.x, y = current.in.y), color = "firebrick1",size=3) +
    theme_minimal() + facet_wrap(~type) + 
    scale_fill_viridis(limits=c(0,NA),na.value="white", name="Environmental\nvalue") + 
    scale_color_viridis(option="magma",limits=c(0,NA),na.value="white", name="Time")
}

## ----setupA, eval = FALSE-----------------------------------------------------
#  SimulationSingle <- nosoiSim(type="single", popStructure="continuous", ...)

## ----setupB, eval = FALSE-----------------------------------------------------
#  SimulationSingle <- nosoiSim(type="single", popStructure="continuous",
#                               length.sim=300, max.infected=1000, init.individuals=1, ...)

## ----setupA-dual, eval = FALSE------------------------------------------------
#  SimulationDual <- nosoiSim(type="dual", popStructure="continuous", ...)

## ----pExit1, eval = FALSE-----------------------------------------------------
#  p_Exit_fct  <- function(t, current.env.value){
#    if(current.env.value > 60){p=0.02}
#    if(current.env.value < 60 && current.env.value > 30){p=0.04}
#    if(current.env.value < 30){p=0.08}
#    return(p)
#  }

## ----pExit2, echo=FALSE, message=FALSE----------------------------------------
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Package 'ggplot2' is needed for plotting this figure.")
} else {
  library(ggplot2)
  library(dplyr)
  
  p_Exit_fctx  <- function(x){
    if(x >= 60){p=0.02}
    if(x < 60 && x > 30){p=0.04}
    if(x <= 30){p=0.08}
    return(p)
  }
  
  data3=data.frame(v2=seq(from = 0, to = 70, by = 0.2))
  
  data3 = data3 %>% group_by(v2) %>% mutate(p=p_Exit_fctx(v2))
  
  ggplot(data=data3,aes(x=v2,y=p)) + geom_line() +  theme_minimal() + labs(x="Environmental value",y="pExit") + ylim(0,0.1)
}

## ----pMove1, eval = FALSE-----------------------------------------------------
#  p_Move_fct  <- function(t){return(0.1)}

## ----sdMove1, eval=FALSE------------------------------------------------------
#  sd_Move_fct  <- function(t){return(0.25)}

## ----nContact1, eval = FALSE--------------------------------------------------
#  n_contact_fct <- function(t){abs(round(rnorm(1, 0.5, 1), 0))}

## ----nContact2, echo=FALSE----------------------------------------------------
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

## ----pTrans1, eval = FALSE----------------------------------------------------
#  p_Trans_fct <- function(t, p_max, t_incub){
#      if(t < t_incub){p=0}
#      if(t >= t_incub){p=p_max}
#      return(p)
#  }

## ----pTrans2, eval = FALSE----------------------------------------------------
#  t_incub_fct <- function(x){rnorm(x,mean = 7,sd=1)}
#  p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}

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

## ----pTrans4, eval = FALSE----------------------------------------------------
#  t_incub_fct <- function(x){rnorm(x,mean = 7,sd=1)}
#  p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}
#  
#  param_pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct)

## ----setupF-------------------------------------------------------------------
library(nosoi)

#Raster is test.raster

#Starting position will be
start.pos <- c(0,0) # c(x,y)

#pExit
p_Exit_fct  <- function(t, current.env.value){
  if(current.env.value > 60){p=0.02}
  if(current.env.value < 60 && current.env.value > 30){p=0.04}
  if(current.env.value < 30){p=0.08}
  return(p)
}

#pMove
p_Move_fct  <- function(t){return(0.1)}

#sdMove
sd_Move_fct  <- function(t){return(0.25)}

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
SimulationSingle <- nosoiSim(type="single", popStructure="continuous",
                             length.sim=300, max.infected=300, init.individuals=1, 
                             
                             init.structure=start.pos, 
                             
                             structure.raster=test.raster,
                             
                             pExit = p_Exit_fct,
                             param.pExit = NA,
                             timeDep.pExit=FALSE,
                             diff.pExit=TRUE,
                             
                             pMove = p_Move_fct,
                             param.pMove = NA,
                             timeDep.pMove=FALSE,
                             diff.pMove=FALSE,
                             
                             sdMove = sd_Move_fct,
                             param.sdMove = NA,
                             timeDep.sdMove=FALSE,
                             diff.sdMove=FALSE,
                             
                             attracted.by.raster=FALSE,
                             
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

## ----pExit1-dual, eval = FALSE------------------------------------------------
#  p_Exit_fctB  <- function(t,prestime){(sin(prestime/(2*pi*10))+1)/16} #for a periodic function

## ----pExit2-dual, echo = FALSE------------------------------------------------
p_Exit_fctx <- function(x){(sin(x/(2*pi*10))+1)/16} #for a periodic function
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Package 'ggplot2' is needed for plotting this figure.")
} else {
  ggplot(data=data.frame(x=0), aes(x=x)) + stat_function(fun=p_Exit_fctx) + theme_minimal() + labs(x="Absolute time (prestime)",y="pExit") + xlim(0,360)
}

## ----pMove.B, eval = FALSE----------------------------------------------------
#  p_Move_fct.B  <- NA

## ----sdMove.B, eval = FALSE---------------------------------------------------
#  sd_Move_fct.B  <- NA

## ----nContact1.B, eval = FALSE------------------------------------------------
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

## ----pTrans1.B, eval = FALSE--------------------------------------------------
#  p_Trans_fct.B <- function(t, max.time){
#    dnorm(t, mean=max.time, sd=2)*5
#  }

## ----pTrans2.B, eval = FALSE--------------------------------------------------
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

## ----pTrans4.B, eval = FALSE--------------------------------------------------
#  max.time_fct <- function(x){rnorm(x,mean = 5,sd=1)}
#  
#  param_pTrans.B = list(max.time=max.time_fct)

## ----setupF.B-----------------------------------------------------------------
library(nosoi)

#Raster is test.raster

#Starting position will be
start.pos <- c(0,0) # c(x,y)

#Host A -----------------------------------

#pExit
p_Exit_fct  <- function(t, current.env.value){
  if(current.env.value > 60){p=0.02}
  if(current.env.value < 60 && current.env.value > 30){p=0.04}
  if(current.env.value < 30){p=0.08}
  return(p)
}

#pMove
p_Move_fct  <- function(t){return(0.1)}

#sdMove
sd_Move_fct  <- function(t){return(0.25)}

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

#pMove
sd_Move_fct.B  <- NA

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
SimulationDual <- nosoiSim(type="dual", popStructure="continuous",
                        length.sim=300, 
                        max.infected.A=100,
                        max.infected.B=200,
                        init.individuals.A=1,
                        init.individuals.B=0,
                        init.structure.A=start.pos,
                        init.structure.B=NA,
                        structure.raster.A=test.raster,
                        structure.raster.B=test.raster,

                        pExit.A = p_Exit_fct,
                        param.pExit.A = NA,
                        timeDep.pExit.A=FALSE,
                        diff.pExit.A=TRUE,
                        
                        pMove.A = p_Move_fct,
                        param.pMove.A = NA,
                        timeDep.pMove.A=FALSE,
                        diff.pMove.A=FALSE,
                        
                        sdMove.A = sd_Move_fct,
                        param.sdMove.A = NA,
                        timeDep.sdMove.A=FALSE,
                        diff.sdMove.A=FALSE,
                        attracted.by.raster.A=FALSE,
                        
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
                        param.pExit.B = NA,
                        timeDep.pExit.B=TRUE,
                        diff.pExit.B=FALSE,
                        
                        pMove.B = p_Move_fct.B,
                        param.pMove.B = NA,
                        timeDep.pMove.B=FALSE,
                        diff.pMove.B=FALSE,
                        
                        sdMove.B = sd_Move_fct.B,
                        param.sdMove.B = NA,
                        timeDep.sdMove.B=FALSE,
                        diff.sdMove.B=FALSE,
                        attracted.by.raster.B=FALSE,
                        
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

