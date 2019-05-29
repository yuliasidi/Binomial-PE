nested.mi.comb.wn <- function(df){
  
 #this is a plug-in WN method
  sum.all <- df%>%
    dplyr::summarise(qbar.c = mean(C_phat), qbar.t = mean(T_phat),
                     n.c = mean(C_n), n.t = mean(T_n))%>%
    dplyr::mutate(qbar.d = qbar.c - qbar.t,
                  z = qnorm(1-alpha),
                  l.c = (qbar.c + z^2/(2*n.c) - 
                           z*sqrt((qbar.c*(1-qbar.c)+z^2/(4*n.c))/n.c))/(1+z^2/n.c),
                  u.c = (qbar.c + z^2/(2*n.c) + 
                           z*sqrt((qbar.c*(1-qbar.c)+z^2/(4*n.c))/n.c))/(1+z^2/n.c),
                  
                  l.t = (qbar.t + z^2/(2*n.t) - 
                           z*sqrt((qbar.t*(1-qbar.t)+z^2/(4*n.t))/n.t))/(1+z^2/n.t),
                  u.t = (qbar.t + z^2/(2*n.t) + 
                           z*sqrt((qbar.t*(1-qbar.t)+z^2/(4*n.t))/n.t))/(1+z^2/n.t),
                  
                  ci.l = qbar.d-sqrt((qbar.c-l.c)^2+(u.t-qbar.t)^2),
                  ci.u = qbar.d+sqrt((u.c-qbar.c)^2+(qbar.t-l.t)^2)
    )%>%
    dplyr::select(qbar.c, qbar.t, qbar.d, ci.l, ci.u)
 
    return(sum.all)
      
}

