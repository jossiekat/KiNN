example3 <-
function(filename)
{
  load(file= filename);
  
  plot(df[,"x"],df[,"y"])
  
  inTrain<-createDataPartition(y=df$y,p=0.7,list=FALSE)
  as.matrix(df$y[inTrain],ncol=1)->Y
  as.matrix(df$y[-inTrain],ncol=1)->y
  as.matrix(df$x[inTrain],ncol=1)->X
  as.matrix(df$x[-inTrain],ncol=1)->x
  
  gmodel<-buildGraphModel(X,Y)
  predict.graph(gmodel,x)->ygraph
  plot(X,Y)
  #predict.graph.2d(gmodel,x)->ygraph
  plotGraphs(gmodel,filename)
  message("corralation to true y : ",cor(y,ygraph))
  
}
