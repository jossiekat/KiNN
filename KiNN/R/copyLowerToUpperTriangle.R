copyLowerToUpperTriangle <-
function(m)
{
  m[upper.tri(m)] <-t(m)[upper.tri(m)]
  return (m)
}
