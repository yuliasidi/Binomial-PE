mi.comb <- function(df){
  df%>%
    summarise(qbar = mean(phat.d),
              ubar = mean(var.d),
              B = var(phat.d),
              num.m = n())%>%
    mutate(T.var = ubar + (num.mi+1)/num.mi*B, 
           v = floor((num.mi - 1)*(1 + ubar/((1+1/num.mi)*B))^2),
           
           ci.l = qbar - qt(1-alpha, v)*sqrt(T.var),
           ci.u = qbar + qt(1-alpha, v)*sqrt(T.var))
}

