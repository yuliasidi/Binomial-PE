# Adds continutios variable X from N(4,1), such that the correlation is equal to rho
add.X <- function(df, rho=0.3, rho.bound){
  df.C <- df%>%
    filter(trt=="C")%>%
    mutate(X = rnorm(length(trt),4,1))%>%
    mutate(X = case_when(X<0 ~ 0, 
                         X>10 ~ 10, 
                         TRUE ~ as.numeric(X)))
  df.T <- df%>%
    filter(trt=="T")%>%
    mutate(X = rnorm(length(trt),4,1))%>%
    mutate(X = case_when(X<0 ~ 0, 
                         X>10 ~ 10, 
                         TRUE ~ as.numeric(X)))
  
  t.order.C <- df.C%>%
    slice(1:floor(abs(rho)*n()/abs(rho.bound)))
  t.asis.C  <- df.C%>%
    slice(floor(abs(rho)*n()/abs(rho.bound))+1:n())
  
  t.order.T <- df.T%>%
    slice(1:floor(abs(rho)*n()/abs(rho.bound)))
  t.asis.T  <- df.T%>%
    slice(floor(abs(rho)*n()/abs(rho.bound))+1:n())
  
  if (rho.bound>0){
    t.ordered.C <- bind_cols(t.order.C%>%select(-X)%>%arrange(y), 
                             t.order.C%>%select(X)%>%arrange(X))
    t.ordered.T <- bind_cols(t.order.T%>%select(-X)%>%arrange(y), 
                             t.order.T%>%select(X)%>%arrange(X))
  } 
  
  if (rho.bound<0){
    t.ordered.C <- bind_cols(t.order.C%>%select(-X)%>%arrange(desc(y)), 
                             t.order.C%>%select(-y)%>%arrange(X)%>%select(X))
    t.ordered.T <- bind_cols(t.order.T%>%select(-X)%>%arrange(desc(y)), 
                             t.order.T%>%select(-y)%>%arrange(X)%>%select(X))
  }
  
  df3 <- bind_rows(t.ordered.T, t.ordered.C, t.asis.T, t.asis.C)
  
  return(df3)
}
