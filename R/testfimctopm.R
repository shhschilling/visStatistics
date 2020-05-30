test=function(a=2,b=2*a)
{
  if (missing(a)) {a=100} 
  c(a,b)
}
test()
