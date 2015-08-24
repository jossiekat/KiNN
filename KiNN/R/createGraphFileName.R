createGraphFileName <-
function(filename,i)
{
  prefix = str_extract(filename, "([a-zA-Z0-9]+)")
  graphname= str_c(prefix,as.character(i),".","gml")
  return (graphname)
}
