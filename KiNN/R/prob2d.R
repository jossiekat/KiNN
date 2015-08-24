prob2d <-
function(cxy)
{
  if (length(cxy)==0)
    return (0)
  table(cxy)->t
  pv<-c(0)
  for (i in 1:length(t))
  {
    pv[i]<-t[i]/sum(t)
  }
  
  return(pv)
}
