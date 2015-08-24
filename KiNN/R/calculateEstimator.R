calculateEstimator <-
function (g,simv,Y,alpha)
{
  neighborsInGraph(g,simv)->ng
  apply(ng,1,function(x) (x%*%Y)/sum(x!=0))->ngy
  #correcting in case no neighbors are present.
  ngy[is.na(ngy)]<-(Y[simv])[is.na(ngy)]
  (1-alpha)* ngy +alpha * t(Y[simv])
}
