
  X<-as.matrix(c(1,2,3,4,5,6),ncol=1)
  Y<-as.matrix(c(1,2,3,4,5,6),ncol=1)
  
  learn<-as.data.frame(cbind(X,Y))
  colnames(learn)<-c("X","Y")
  x<-as.matrix(c(2.5,3.8,5.5),ncol=1)
  test<-as.data.frame(x)
  colnames(test)<-c("X")
  plot(learn$X,learn$Y,pch=16,xlab = "X",ylab = "Y")

  gmodel<-kinn(Y~X,learn) 
  test$Y<-predict.kinn(gmodel,test)
  message("adding predicted points to graph")
  points(test$X,test$Y,pch=5)
  

