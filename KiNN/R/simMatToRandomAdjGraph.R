simMatToRandomAdjGraph <-
function(mat,nedges,directed)
{ 
  mat<-eliminateSelfSimilarity(mat)
  
  if( directed ==  FALSE)
    m<-eliminateUpperTriangle(mat);
  mat<-normMatrixToExpEdges(mat,nedges,directed)
  mat<-apply(mat, 1:2, function(x) biasedCoinToss(x))
  
  if( directed ==  FALSE)
    mat<-copyLowerToUpperTriangle(mat);
  
  return (mat)
  
}
