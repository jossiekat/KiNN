predict.kinn <-
function(g,data)
{
  X=as.matrix(data[,g$indep])
  predict(g$mX,X)$classification->zx
  Y<-rep(0,length(X))
  for (i in 1:g$mX$G)
  {  
    if (i %in% zx)
    {
      e<-estimate(g,i,X[i==zx])
      Y[i==zx]<-e
    }
    
  }
  print(Y)
  return (Y)
}
