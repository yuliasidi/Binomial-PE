nested.mi.comb <- function(df){
  
  #summaries per imputation model
  sum.by.m <- 
    df%>%
    dplyr::group_by(m)%>%
    dplyr::summarise(qbar.m = mean(phat.d),
              q.m.var = var(phat.d),
              ubar.m = mean(var.d),
              n = n())
    
  #overall summary
  sum.all <- 
      sum.by.m%>%
      dplyr::summarise(qbar = mean(qbar.m),
                     ubar = mean(ubar.m),
                     b = var(qbar.m),
                     w = mean(q.m.var),
                     n = mean(n),
                     m = n())%>%
      dplyr::mutate(t = ubar + (1 + 1/m)*b + (1 - 1/n)*w,
                    v_1 = ((1 + 1/m)*b/t)^2/(m-1) + ((1 - 1/n)*w/t)^2/(m*(n-1)),
                    v = floor(1/v_1)
                    )%>%
      dplyr::mutate(ci.l = qbar - qt(1-alpha, v)*sqrt(t),
                    ci.u = qbar + qt(1-alpha, v)*sqrt(t))
    
    return(sum.all)
      
}

