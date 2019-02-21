

pat <- expand.grid(
  MISSING=c('mcar','mar1','mar2','mar3','mar4','mar5','mar6','mnar1','mnar2'),
  PERCENT=sprintf('%02d',seq(5,25,5))
  ,stringsAsFactors = FALSE)
pat <- pat%>%
  dplyr::mutate(pattype = sprintf("%s%s",MISSING, PERCENT),
                i = seq(0,44,1))
