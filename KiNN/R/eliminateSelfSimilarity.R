eliminateSelfSimilarity <-
function(mat)
{
  mat<-mat-diag(mat)*diag(nrow(mat))
}
