numEdgesBasedOnNoise <-
function(x,y,uue="linear")
{
  if (uue == "linear")
      noise<-unexpLinearNoise (x,y)
  else
      noise<-unexpSplineNoise(x,y)
  
  n<-nrow(as.matrix(x))
  edges<-max(noise*n*(n-1)/2,n)
  return (edges)
  
}
