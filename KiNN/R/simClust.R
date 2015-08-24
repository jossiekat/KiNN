simClust <-
function(data,ngraph=10)
{

  mixclust = Mclust(data,G=1:ngraph)
  return (mixclust)
}
