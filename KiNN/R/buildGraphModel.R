buildGraphModel <-
function(X,Y)
{
  listModel<-list()
  simClust(X)->mX
  XY = cbind(X,Y)
  simClust(XY)->mXY
  c2d<-mXY$classification
  gx<-list()
  vx<-list()
  vy<-list()
  cxy<-list()
  
  for ( i in 1:mX$G)
  {   
    id = (mX$classification == i)
    gx[[i]] = buildGraph(X[id],Y[id])
    vx[[i]]=X[id]
    vy[[i]]=Y[id]
    cxy[[i]]=c2d[id]
    #print(gx[[i]])
  }
  
  listModel$mX = mX
  listModel$mXY = mXY
  listModel$gx<-gx
  listModel$vx<-vx
  listModel$vy<-vy
  listModel$X<-X
  listModel$Y<-Y
  listModel$cxy<-cxy
  
  return (listModel)
  
}
