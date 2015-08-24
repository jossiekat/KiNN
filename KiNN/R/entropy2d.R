entropy2d <-
function(cxy)
{
  if (length(cxy)==0)
    return (0)
  table(cxy)->t
  e<-0
  pv<-0
  pe<-list()
  for (i in 1:length(t))
  {
    p<-t[i]/sum(t)
    e<-e+log2(1/p)*p
  }
  if (e!=0)
      e<-e/log2(length(t))
  return(round(e,3))
}
