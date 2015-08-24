simMatToDetermAdjGraph <-
function(mat,nedges,directed)
{ 
  mat<-eliminateSelfSimilarity(mat)
  mat<-topValuesDeterminsticEdges(mat,nedges,directed)
  return(mat)
}
