unexpSplineNoise <-
function(x,y)
{  
  smooth.spline(x, y)->sp
  predict(sp,x)->p
  
  return (var(y-p$y)/var(y))
}
