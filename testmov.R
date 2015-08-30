

#suppressPackageStartupMessages(library(KRLS))
library(mclust) # for em
library(caret)
library(caTools)
library(igraph)
library(KRLS)
library(KiNN)





cat("load movies data.\n")
load(file="mov.Rdata")
cat("pre process data...\n")
cat("convert budget and gross to log10 so that predcted y will be closer.\n")
#cat("does not pass shapiro wilk test for normality\n")
mov["log10.budget.n"]<-log(mov["budget.n"],10)
mov["log10.total.gross.n"]<-log(mov["total.gross.n"],10)
y<-mov$log10.total.gross.n
x<-rep(0,length(y))

dfxy<-data.frame(y,x)


cat("partition to training and testing groups(catools package).\n")

inTrain = sample.split(dfxy$y, SplitRatio = 0.7) #catools
#createDataPartition(y=dfxy$y,p=0.7,list=FALSE)->inTrain

cat("linear regression on training group.\n")
m=lm(log10.total.gross.n ~ log10.budget.n+MPAA.rating,data=mov[inTrain,])
summary(m)


cat("create data frame x = lm$fitted ")
dfxy[inTrain,"x"]<-m$fitted.values
x[inTrain]<-m$fitted.values
y[inTrain]->training
y[-inTrain]->testing

predict(m,newdata=mov[-inTrain,])->p
p->x[-inTrain]
dfxy[-inTrain,"x"]<-p




cat("predict x class testing group.\n")




dfxy[,"xsimidx"]<-NA
dfxy[,"xsim"]<-NA
dfxy[,"ysim"]<-NA
dfxy[,"ygraph"]<-NA
dfxy[,"group"]<-NA
dfxy[inTrain,"group"]<-"L"
dfxy[-inTrain,"group"]<-"T"
dfxy[inTrain,"truey"]<-mov[inTrain,"total.gross.n"]
dfxy[-inTrain,"truey"]<-mov[-inTrain,"total.gross.n"]



cat("KiNN model...training group")
kinnmodel<-kinn(y~x,dfxy[inTrain,])

cat("linear model, training group.\n")
lmodel<-lm(y~x,data=dfxy[inTrain,])
summary(lmodel)

ytgraph<-predict.kinn(kinnmodel,dfxy[inTrain,])

dfxy[inTrain,"graph"]<-ytgraph


cat("graph model, training group.\n")
lgmodel<-lm(y~x+graph,data=dfxy[inTrain,])
summary(lgmodel)



ptest<-predict(lmodel,newdata=dfxy[-inTrain,])


ygraph<-predict.kinn(kinnmodel,dfxy[-inTrain,])
cat("kinn model-correlation, testing group.\n")
cor(ygraph,dfxy[-inTrain,"y"])

dfxy[-inTrain,"graph"]<-ygraph
pgraph<-predict(lgmodel,newdata=dfxy[-inTrain,])


cat("linear model-correlation, testing group.\n")
cor(ptest,dfxy[-inTrain,"y"])


cat("graph model-correlation, testing group.\n")
cbind(dfxy[-inTrain,"y"],pgraph)->l
cor(l[,1],l[,2])



ggplot(dfxy[-inTrain,], aes(x)) + 
  geom_point(aes(y = y, colour = "test y")) + 
  geom_point(aes(y = pgraph, colour = "graph model"))+
  geom_point(aes(y = ptest,  colour = "linear model"))+ scale_colour_discrete(name  = "Models") # Change legend title 


cat("correlation with true y(y was pre processed to log10.\n")
cat("linear model-correlation, exp(y) testing group.\n")
cbind(dfxy[-inTrain,"truey"],ptest)->l
cor(l[,1],exp(l[,2]))

cat("graph model-correlation, exp(y) testing group.\n")
cbind(dfxy[-inTrain,"truey"],ygraph)->l
cor(l[,1],exp(l[,2]))

#View(dfxy)



