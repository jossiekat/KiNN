buildGraph <-
function(X,Y,method="det",uue="linear")
{
  mat<-buildSimilarityMatrix(X)
  nedges = numEdgesBasedOnNoise(X,Y,uue)
  if (method == "det")
      g<-simMatToDetermAdjGraph(mat,nedges,directed=F)
  
  return(g)
}
