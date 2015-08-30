
unexpLinearNoise <-function(x,y)
{
  m<-lm(y~x)
  summary(m)->s
  return (1-s$r.squared)  
}

unexpSplineNoise <-
  function(x,y)
  {  
    smooth.spline(x, y)->sp
    predict(sp,x)->p
    
    return (var(y-p$y)/var(y))
  }

numEdgesBasedOnNoise <-
function(x,y,uue="linear")
{
  if (uue == "linear")
    noise<-unexpLinearNoise(x,y)
  else
      noise<-unexpSplineNoise(x,y)
  
  n<-nrow(as.matrix(x))
  edges<-max(noise*n*(n-1)/2,n)
  return (edges)
  
}
