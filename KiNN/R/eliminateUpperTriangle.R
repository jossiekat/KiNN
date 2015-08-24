eliminateUpperTriangle <-
function(m)
{
  m[upper.tri(m)]<- 0
  return (m)
}
