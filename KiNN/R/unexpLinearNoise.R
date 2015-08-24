unexpLinearNoise <-
function(x,y)
{
  m<-lm(y~x)
  summary(m)->s
  return (1-s$r.squared)  
}
