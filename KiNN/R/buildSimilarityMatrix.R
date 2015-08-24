buildSimilarityMatrix <-
function(x,sigma=1)
{
  mat<-gausskernel(X = x,sigma = sigma)
  return (mat)
}
