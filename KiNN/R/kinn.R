kinn <-
function ( f,data )
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
