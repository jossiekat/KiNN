
topValuesDeterminsticEdges <-
  function(m,nedges,directed)
  {
    
    if (directed==FALSE)  
      nedges<-nedges * 2
    
    r<-rank(m,ties.method= "first")
    topr<-(length(r) - nedges + 1)
    w<-which(r<topr)
    r[w]<-0
    r[-w]<-1
    g<-matrix(r,nrow=nrow(m))
    
    return (g)
  }

simMatToDetermAdjGraph <-
function(mat,nedges,directed)
{ 
  mat<-eliminateSelfSimilarity(mat)
  mat<-topValuesDeterminsticEdges(mat,nedges,directed)
  return(mat)
}
