biasedCoinToss <-
function(p)
{
  toss<-as.numeric(runif(1)) 
  if ( toss > p) 
    return (0)  
  return (1)
}
