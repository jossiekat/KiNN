exmaple2 <-
function()
{
  X<-as.matrix(c(1,2,3,1,2,3),ncol=1)
  Y<-as.matrix(c(1,2,3,6,5,4),ncol=1)
  x<-as.matrix(c(2.5,3.8,5),ncol=1)
  gmodel<-buildGraphModel(X,Y)
  predict.graph(gmodel,x)->ygraph
  plot(X,Y)
  plotGraphs(gmodel)
}
