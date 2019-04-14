miss.impose.x2.cont <- function(df, b.trt = log(1), b.Y = log(1), b.X1 = log(1), b.X2 = log(1), do = 0.1, seed)
  {
  
  tmp <- df%>%
    dplyr::summarise_at( .vars = c('trtn', 'x1', 'x2', 'y'), .funs= list(mean="mean"))%>%
    dplyr::mutate(int = log(do/(1-do)) - b.trt*trtn_mean - b.Y*y_mean - b.X1*x1_mean/10 - b.X2*x2_mean/100)
      
  set.seed(seed)
  
  dt.miss <- df%>%
    dplyr::mutate(p = 1/(1+exp(-1*(tmp$int + b.trt*trtn + b.Y*y + b.X1*x1/10 + b.X2*x2/100))),
                  p.thresh = runif(n()))%>%
    dplyr::mutate(y.m = ifelse(p >= p.thresh,NA_integer_,y))%>%
    dplyr::select(-c(p, p.thresh, y))
  
  return(dt.miss) 
}
