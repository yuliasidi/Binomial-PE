mi.comb <- function(df){
  df%>%
    summarise(qbar = mean(phat.d),
              ubar = mean(var.d),
              B = var(phat.d),
              m = n())%>%
    mutate(T.var = ubar + (m+1)/m*B, 
           v = floor((m - 1)*(1 + ubar/((1+1/m)*B))^2),
           
           ci.l = qbar - qt(1-alpha, v)*sqrt(T.var),
           ci.u = qbar + qt(1-alpha, v)*sqrt(T.var))
}

