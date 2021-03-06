#libraries and parameters

suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(caTools))
suppressPackageStartupMessages(library(KRLS))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(mclust))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(formula.tools))



#plot better igraphs and use special program for it.
#make a package.
#make a doc
#enable  data frames.
#check mov
#better output
#cut y to 5 decimals.
#arrange test program
#remove prints.
#add point to calc


#signif(1.128347132904321674821,digits=3)

# normalizing matrix so expectation of the sum of all probabilities 
# will be the number of edges in graph.
normMatrixToExpEdges <- function (mat,nedges,directed)
{
  s <- sum(mat)
  proportion <- nedges / s
  if (directed == FALSE) proportion <-(proportion * 2) 
  mat <-proportion * mat
  print(sum(mat))
  print(mat)
  return(mat)
}

#biased coin toss, mind that in case p > 1 it will also "work"
biasedCoinToss<-function(p)
{
  toss<-as.numeric(runif(1)) 
  if ( toss > p) 
    return (0)  
  return (1)
}

#zeroing the diagonal of the similarity matrix
eliminateSelfSimilarity <-function(mat)
{
  mat<-mat-diag(mat)*diag(nrow(mat))
}

#just like it's says.
eliminateUpperTriangle <-function(m)
{
  m[upper.tri(m)]<- 0
  return (m)
}

#handling the case of symetric matrix.
copyLowerToUpperTriangle <-function(m)
{
  m[upper.tri(m)] <-t(m)[upper.tri(m)]
  return (m)
}
#create adjency graph matrix from
#similarity matrix probablistic
simMatToRandomAdjGraph <-function(mat,nedges,directed)
{ 
  mat<-eliminateSelfSimilarity(mat)
  
  if( directed ==  FALSE)
    m<-eliminateUpperTriangle(mat);
  mat<-normMatrixToExpEdges(mat,nedges,directed)
  mat<-apply(mat, 1:2, function(x) biasedCoinToss(x))
  
  if( directed ==  FALSE)
    mat<-copyLowerToUpperTriangle(mat);
  
  return (mat)
  
}

topValuesDeterminsticEdges<-function(m,nedges,directed)
{
  
  if (directed==FALSE)  
    nedges<-nedges * 2
  
  r<-rank(m,ties.method= "first")
  topr<-(length(r) - nedges + 1)
  w<-which(r<topr)
  r[w]<-0
  r[-w]<-1
  g<-matrix(r,nrow=nrow(m))
  
  return (g)
}



unexpSplineNoise <-function(x,y)
{  
  smooth.spline(x, y)->sp
  predict(sp,x)->p
  
  return (var(y-p$y)/var(y))
}
unexpLinearNoise <-function(x,y)
{
  m<-lm(y~x)
  summary(m)->s
  return (1-s$r.squared)  
}
numEdgesBasedOnNoise<-function(x,y,uue="linear")
{
  if (uue == "linear")
      noise<-unexpLinearNoise (x,y)
  else
      noise<-unexpSplineNoise(x,y)
  
  n<-nrow(as.matrix(x))
  edges<-max(noise*n*(n-1)/2,n)
  return (edges)
  
}
#deteministic
simMatToDetermAdjGraph <-function(mat,nedges,directed)
{ 
  mat<-eliminateSelfSimilarity(mat)
  mat<-topValuesDeterminsticEdges(mat,nedges,directed)
  return(mat)
}

buildGraph<-function(X,Y,method="det",uue="linear")
{
  mat<-buildSimilarityMatrix(X)
  nedges = numEdgesBasedOnNoise(X,Y,uue)
  if (method == "det")
      g<-simMatToDetermAdjGraph(mat,nedges,directed=F)
  
  return(g)
}






buildSimilarityMatrix<-function(x,sigma=1)
{
  mat<-gausskernel(X = x,sigma = sigma)
  return (mat)
}




mostSimilarIndices<-function(X,x)
{
 
  s<-buildSimilarityMatrix(rbind(X,x))
  s<-s[(nrow(X)+1):nrow(s),1:nrow(X),drop=F]
  apply(s,1,function(l) which(l == max(l))[1])->simv
  #simvec<-simvec[2:length(simvec)]
  return (simv)
}


neighborsInGraph<-function(g,simv)
{
  #if (simv == NULL)
  #if ((length(simv)==0) || (simv == NULL))
  return(g[simv,])
}

calculateEstimator<-function (g,simv,Y,alpha)
{
  neighborsInGraph(g,simv)->ng
  apply(ng,1,function(x) (x%*%Y)/sum(x!=0))->ngy
  #correcting in case no neighbors are present.
  ngy[is.na(ngy)]<-(Y[simv])[is.na(ngy)]
  (1-alpha)* ngy +alpha * t(Y[simv])
}

entropy2d<-function(cxy)
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

prob2d<-function(cxy)
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

getEstimatorsVector<-function (g,simv,Y,C,alpha)
{
  y2d<-list()
  neighborsInGraph(g,simv)->ng
  ent<-0
  probv<-list()
  t(apply(ng,1,function(x) (x*Y)))->ngvy
  t(apply(ng,1,function(x) (x*C)))->ngvc
  for (i in 1:nrow(ngvc))
  {
    ngvc[i,]->line
    line[line > 0]->line
    ent[i]<-entropy2d(line)  
    probv[[i]]<-prob2d(line)
  }
 
  
  for (i in 1:nrow(ngvc))
  {
    for (j  in 1:max(C))
    {
      Y[ngvc[i,]==j]->y2d[[j]]       
    }
    if (ent[i]>0.5)
        sig<-"*"
    else
      if (ent[i]>0.2)
        sig<-"."
    else
        sig<-" "
    
    ye<-vector()
    if (probv[[i]]>0) 
    {  k=1
      for (j  in 1:max(ngvc[i,]))
      {  
      if (length(y2d[[j]] ) > 0)
      {
      ye[k]<-mean(y2d[[j]])
      k=k+1
      }
      }

      
      if (ent[i]==0)
      {
        s<-paste0("p=",round(probv[[i]],3),",yhat=",round(as.numeric(ye),3))
        cat(sig," ","entropy=",ent[i],s,"\n")
      }
      else
      {
      s<-paste0("(p=",round(probv[[i]],3),",yhat=",round(ye,3),")")
      cat(sig," ","entropy=",ent[i],s,"\n")
      }
    }
    else
    { sig=" "
      s<-paste0("p=",1)
      cat(sig," ","entropy=",0,s,"-no neighbors in graph.\n")
      
     }
  }
  #print(ngvc)
  return (ngvc)
  
  
}
simClust<-function(data,ngraph=10)
{

  mixclust = Mclust(data,G=1:ngraph)
  return (mixclust)
}
#create model list.


buildGraphModel<-function(X,Y)
{
  listModel<-list()
  simClust(X)->mX
  XY = cbind(X,Y)
  simClust(XY)->mXY
  c2d<-mXY$classification
  gx<-list()
  vx<-list()
  vy<-list()
  cxy<-list()
  
  for ( i in 1:mX$G)
  {   
    id = (mX$classification == i)
    gx[[i]] = buildGraph(X[id],Y[id])
    vx[[i]]=X[id]
    vy[[i]]=Y[id]
    cxy[[i]]=c2d[id]
    #print(gx[[i]])
  }
  
  listModel$mX = mX
  listModel$mXY = mXY
  listModel$gx<-gx
  listModel$vx<-vx
  listModel$vy<-vy
  listModel$X<-X
  listModel$Y<-Y
  listModel$cxy<-cxy
  
  return (listModel)
  
}


estimate<-function(g,i,x)
{

  simv<-mostSimilarIndices(matrix(g$vx[[i]]),matrix(x))
  calculateEstimator(g$gx[[i]],simv,g$vy[[i]],0.8)->est
  ev<-getEstimatorsVector(g$gx[[i]],simv,g$vy[[i]],g$cxy[[i]],0.8)
  return (est)
  
}


predict.graph<-function(g,X)
{
  predict(g$mX,X)$classification->zx
  Y<-rep(0,length(X))
  for (i in 1:g$mX$G)
  {  
    if (i %in% zx)
    {
    e<-estimate(g,i,X[i==zx])
    Y[i==zx]<-e
    }
        
  }
  print(Y)
  return (Y)
}


predict.kinn<-function(g,data)
{
  X=as.matrix(data[,g$indep])
  predict(g$mX,X)$classification->zx
  Y<-rep(0,length(X))
  for (i in 1:g$mX$G)
  {  
    if (i %in% zx)
    {
      e<-estimate(g,i,X[i==zx])
      Y[i==zx]<-e
    }
    
  }
  print(Y)
  return (Y)
}


createGraphFileName<-function(filename,i)
{
  prefix = str_extract(filename, "([a-zA-Z0-9]+)")
  graphname= str_c(prefix,as.character(i),".","gml")
  return (graphname)
}
plotGraphs<-function(gmodel,filename)
{
  for ( i in 1:length(gmodel$gx))
  {
    m=gmodel$gx[[1]]
    g=graph.adjacency(m,mode="undirected",weighted=NULL)
    gfilename = createGraphFileName(filename,i)
    write.graph(g, gfilename, format=c("gml"))
    plot(g)
  }
}
exmaple<-function()
{
X<-as.matrix(c(1,2,3,4,5,6),ncol=1)
Y<-as.matrix(c(1,2,3,4,5,6),ncol=1)
x<-as.matrix(c(2.5,3.8,5),ncol=1)
gmodel<-buildGraphModel(X,Y)
predict.graph(gmodel,x)->ygraph

plotGraphs(gmodel)
}

exmaple2<-function()
{
  X<-as.matrix(c(1,2,3,1,2,3),ncol=1)
  Y<-as.matrix(c(1,2,3,6,5,4),ncol=1)
  x<-as.matrix(c(2.5,3.8,5),ncol=1)
  gmodel<-buildGraphModel(X,Y)
  predict.graph(gmodel,x)->ygraph
  plot(X,Y)
  plotGraphs(gmodel)
}
exmaple<-function()
{
  X<-as.matrix(c(1,2,3,4,5,6),ncol=1)
  Y<-as.matrix(c(1,2,3,4,5,6),ncol=1)
  x<-as.matrix(c(2.5,3.8,5),ncol=1)
  gmodel<-buildGraphModel(X,Y)
  predict.graph(gmodel,x)->ygraph
  plot(X,Y)
  #predict.graph.2d(gmodel,x)->ygraph
  plotGraphs(gmodel)
}

example3<-function(filename)
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


example4<-function(filename)
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

kinn<-function ( f,data )
{
  g<-as.formula(f)
    
  indep<-all.vars(g)[2]
  if (indep ==  ".")
    indep = setdiff(colnames(data),lhs.vars(g) )
  else
    indep = rhs.vars(g) 
  
  
  X<-as.matrix(data[,indep])
  Y<-as.matrix(data[,lhs.vars(g)])
  graph<-buildGraphModel(X,Y)
  graph$dep<-lhs.vars(g)
  graph$indep<-indep
  return (graph)
}

#exmaple()
#exmaple2()
  
#example3("I.Rda")
#example3("II.Rda")
#example4("III.Rda")
#example3("IV.Rda")
#example3("V.Rda")


message("graph model")




