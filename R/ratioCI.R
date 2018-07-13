ratioCI = function(a=NA, b=NA, sea=NA, seb=NA, na=NA, nb=NA, alpha=0.05){
  q=a/b
  tstar=qt(1-alpha/2, df=na+nb-2)
  g=(tstar*seb/b)^2
  seq=(q/(1-g))*sqrt( (1-g)*(sea^2)/(a^2) + (seb^2)/(b^2) )
  return( c(q/(1-g)-tstar*seq, q/(1-g)+tstar*seq) )
}