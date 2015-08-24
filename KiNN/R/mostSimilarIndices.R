mostSimilarIndices <-
function(X,x)
{
 
  s<-buildSimilarityMatrix(rbind(X,x))
  s<-s[(nrow(X)+1):nrow(s),1:nrow(X),drop=F]
  apply(s,1,function(l) which(l == max(l))[1])->simv
  #simvec<-simvec[2:length(simvec)]
  return (simv)
}
