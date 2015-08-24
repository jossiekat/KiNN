estimate <-
function(g,i,x)
{

  simv<-mostSimilarIndices(matrix(g$vx[[i]]),matrix(x))
  calculateEstimator(g$gx[[i]],simv,g$vy[[i]],0.8)->est
  ev<-getEstimatorsVector(g$gx[[i]],simv,g$vy[[i]],g$cxy[[i]],0.8)
  return (est)
  
}
