require(igraph)
require(lubridate)
require(rgexf)
require(drc)

networkHealthGexf <- function(filegexf) {

  g <- gexf.to.igraph(read.gexf(filegexf))

  teamNodes = as.numeric(V(g)[team==TRUE])
  modEdges = c(unlist(mapply(teamNodes,FUN=function(n){incident(g,n,mode='out')})))
  E(g)$moderator=FALSE
  E(g)[modEdges]$moderator=TRUE
  
  dates=as.POSIXct(E(g)$ts,origin='1970-01-01')
  E(g)$yearweek=year(dates)*100+week(dates)
  
  g$modEvents    = as.data.frame(table(E(g)[moderator==TRUE]$yearweek))
  g$nonModEvents = as.data.frame(table(E(g)[moderator!=TRUE]$yearweek))
  
  g$respOutcome = merge(g$nonModEvents,g$modEvents,by='Var1',all=TRUE)
  g$respOutcome[is.na(g$respOutcome)] = 0
  colnames(g$respOutcome)=c('yearweek','nonModEvents','modEvents')
  
  g$drm=drm(nonModEvents~modEvents, data=g$respOutcome, fct=LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
  g$drmW=drm(nonModEvents~modEvents, data=g$respOutcome, fct=W1.4())
  g$drmE=drm(nonModEvents~modEvents, data=g$respOutcome, fct=EXD.2())
  
  nodeActivity=data.frame(weeksSinceCreated=numeric(),outPosts=numeric(),inPosts=numeric(),nId=numeric())
  for (nId in V(g)) {
    n=V(g)[nId]
    ttOut=as.data.frame(table(round((E(g)[incident(g,nId,mode='out')]$ts-n$created_ts)/(3600*24*7)),useNA='always'),responseName='outPosts')
    ttIn=as.data.frame(table(round((E(g)[incident(g,nId,mode='in')]$ts-n$created_ts)/(3600*24*7)),useNA='always'),responseName='inPosts')
    
    nodeOutIn=merge(ttIn,ttOut,by='Var1',all=TRUE)
    
    nodeOutIn$nId=nId
    
    colnames(nodeOutIn)=c('weekSinceCreated','outPosts','inPosts','nId')
    
    nodeActivity=rbind(nodeActivity,na.omit(nodeOutIn))
  }
  
  g$nodeActivity=nodeActivity
  return (g)
}

networkHealth <- function(g) {
  
  teamNodes = as.numeric(V(g)[team==TRUE])
  modEdges = c(unlist(mapply(teamNodes,FUN=function(n){incident(g,n,mode='out')})))
  E(g)$moderator=FALSE
  E(g)[modEdges]$moderator=TRUE
  
  dates=as.POSIXct(E(g)$ts,origin='1970-01-01')
  E(g)$yearweek=year(dates)*100+week(dates)
  
  g$modEvents    = as.data.frame(table(E(g)[moderator==TRUE]$yearweek))
  g$nonModEvents = as.data.frame(table(E(g)[moderator!=TRUE]$yearweek))
  
  g$respOutcome = merge(g$nonModEvents,g$modEvents,by='Var1',all=TRUE)
  g$respOutcome[is.na(g$respOutcome)] = 0
  colnames(g$respOutcome)=c('yearweek','nonModEvents','modEvents')
  
  g$drm=drm(nonModEvents~modEvents, data=g$respOutcome, fct=LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
  g$drmW=drm(nonModEvents~modEvents, data=g$respOutcome, fct=W1.4())
  g$drmE=drm(nonModEvents~modEvents, data=g$respOutcome, fct=EXD.2())
  
  nodeActivity=data.frame(weeksSinceCreated=numeric(),outPosts=numeric(),inPosts=numeric(),nId=numeric())
  for (nId in V(g)) {
    n=V(g)[nId]
    ttOut=as.data.frame(table(round((E(g)[incident(g,nId,mode='out')]$ts-n$created_ts)/(3600*24*7)),useNA='always'),responseName='outPosts')
    ttIn=as.data.frame(table(round((E(g)[incident(g,nId,mode='in')]$ts-n$created_ts)/(3600*24*7)),useNA='always'),responseName='inPosts')
    
    nodeOutIn=merge(ttIn,ttOut,by='Var1',all=TRUE)
    
    nodeOutIn$nId=nId
    
    colnames(nodeOutIn)=c('weekSinceCreated','outPosts','inPosts','nId')
    
    nodeActivity=rbind(nodeActivity,na.omit(nodeOutIn))
  }
  
  g$nodeActivity=nodeActivity
  return (g)
}


plotNetworkHealth <- function(g)
{
  plot(nonModEvents~modEvents, data=g$respOutcome)
}
