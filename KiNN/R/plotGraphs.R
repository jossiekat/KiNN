plotGraphs <-
function(gmodel,filename)
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
