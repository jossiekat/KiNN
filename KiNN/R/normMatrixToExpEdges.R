normMatrixToExpEdges <-
function (mat,nedges,directed)
{
  s <- sum(mat)
  proportion <- nedges / s
  if (directed == FALSE) proportion <-(proportion * 2) 
  mat <-proportion * mat
  print(sum(mat))
  print(mat)
  return(mat)
}
