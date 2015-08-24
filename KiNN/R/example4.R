example4 <-
function(filename)
{
  load(file= filename);
  
  plot(df[,"x"],df[,"y"])
  
  inTrain<-createDataPartition(y=df$y,p=0.7,list=FALSE)
  train<-df[inTrain,];
  test<-df[-inTrain,]
  
  
  
  gmodel<-kinn("y~x+z",train)
  predict.kinn(gmodel,test)->ygraph
  
  #predict.graph.2d(gmodel,x)->ygraph
  plotGraphs(gmodel,filename)
  message("corralation to true y : ",cor(test$y,ygraph))
  
}
