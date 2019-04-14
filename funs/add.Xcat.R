# Adds continutios variable X from N(4,1), such that the correlation is equal to rho
add.Xcat <- function(df, rho=0.3, rho.bound.C, rho.bound.T){
  df.C <- df%>%
    filter(trt=="C")%>%
    mutate(X = rbinom(length(trt),1,0.5))
  
  df.T <- df%>%
    filter(trt=="T")%>%
    mutate(X = rbinom(length(trt),1,0.5))
  
  t.order.C <- df.C%>%
    slice(1:floor(abs(rho)*n()/abs(rho.bound.C)))
  t.asis.C  <- df.C%>%
    slice(floor(abs(rho)*n()/abs(rho.bound.C))+1:n())
  
  t.order.T <- df.T%>%
    slice(1:floor(abs(rho)*n()/abs(rho.bound.T)))
  t.asis.T  <- df.T%>%
    slice(floor(abs(rho)*n()/abs(rho.bound.T))+1:n())
  
  if (rho.bound.C>0){
    t.ordered.C <- bind_cols(t.order.C%>%select(-X)%>%arrange(y), 
                             t.order.C%>%select(X)%>%arrange(X))
    t.ordered.T <- bind_cols(t.order.T%>%select(-X)%>%arrange(y), 
                             t.order.T%>%select(X)%>%arrange(X))
  } 
  
  if (rho.bound.C<0){
    t.ordered.C <- bind_cols(t.order.C%>%select(-X)%>%arrange(desc(y)), 
                             t.order.C%>%select(-y)%>%arrange(X)%>%select(X))
    t.ordered.T <- bind_cols(t.order.T%>%select(-X)%>%arrange(desc(y)), 
                             t.order.T%>%select(-y)%>%arrange(X)%>%select(X))
  }
  
  df3 <- bind_rows(t.ordered.T, t.ordered.C, t.asis.T, t.asis.C)
  
  return(df3)
}
