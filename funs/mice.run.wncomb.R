mice.run.wncomb<- function(dt, n.mi = 5, method = "logreg", M2){
  
  dt.mice <- dt%>%
    dplyr::select(trt,y.m,X)%>%
    dplyr::mutate(y.m = as.factor(y.m),
                  trt = as.factor(trt))
  
  dt.mice.imp <- mice::mice(
    data = dt.mice,
    m=n.mi,
    method = method,
    seed = 9875,
    maxit = 100,
    printFlag = FALSE
  )
  
  imp.n <- as.list(data.frame(i = seq(1,dt.mice.imp$m,1)))

  dttmp <- purrr::pmap_dfr(imp.n, .f=function(i){
    dt <- mice::complete(dt.mice.imp,i)%>%
      dplyr::mutate(y.m = as.numeric(y.m)-1)
    dt%>%
      dplyr::group_by(trt)%>%
      dplyr::summarise(phat = mean(y.m))%>%
      tidyr::spread(trt, phat)%>%
      dplyr::rename(qhat.c = C, qhat.t = T)%>%
      dplyr::mutate(n.c = nrow(complete(dt.mice.imp,1)%>%filter(trt=="C")),
                    n.t = nrow(complete(dt.mice.imp,1)%>%filter(trt=="T")),
                    u.c = qhat.c/n.c,
                    u.t = qhat.t/n.t)
  }, .id = "i")
  
  mice.res <- dttmp%>%
    dplyr::summarise(qbar.c = mean(qhat.c), qbar.t = mean(qhat.t),
                     ubar.c = mean(u.c), ubar.t = mean(u.t),
                     b.c = var(qhat.c), b.t = var(qhat.t),
                     n.c = mean(n.c), n.t = mean(n.t))%>%
    dplyr::mutate( m = dt.mice.imp$m,
                   rm.c = (1+1/m)*b.c/ubar.c,
                   rm.t = (1+1/m)*b.t/ubar.t,
                   v.c = (m-1)*(1+1/rm.c)^2,
                   v.t = (m-1)*(1+1/rm.t)^2,
                   t.c = qt(1-alpha, v.c),
                   t.t = qt(1-alpha, v.t),
                   
                   qbar.d = qbar.c - qbar.t,
                   
                   lb.c = (2*qbar.c + t.c^2/n.c + t.c^2*rm.c/n.c)/(2*(1 + t.c^2/n.c + t.c^2*rm.c/n.c)) -
                     sqrt((2*qbar.c + t.c^2/n.c + t.c^2*rm.c/n.c)^2/(4*(1 + t.c^2/n.c + t.c^2*rm.c/n.c)^2)-
                            qbar.c^2/(1 + t.c^2/n.c + t.c^2*rm.c/n.c)),
                   ub.c = (2*qbar.c + t.c^2/n.c + t.c^2*rm.c/n.c)/(2*(1 + t.c^2/n.c + t.c^2*rm.c/n.c)) +
                     sqrt((2*qbar.c + t.c^2/n.c + t.c^2*rm.c/n.c)^2/(4*(1 + t.c^2/n.c + t.c^2*rm.c/n.c)^2)-
                            qbar.c^2/(1 + t.c^2/n.c + t.c^2*rm.c/n.c)),
                   
                   lb.t = (2*qbar.t + t.t^2/n.t + t.t^2*rm.t/n.t)/(2*(1 + t.t^2/n.t + t.t^2*rm.t/n.t)) -
                     sqrt((2*qbar.t + t.t^2/n.t + t.t^2*rm.t/n.t)^2/(4*(1 + t.t^2/n.t + t.t^2*rm.t/n.t)^2)-
                            qbar.t^2/(1 + t.t^2/n.t + t.t^2*rm.t/n.t)),
                   ub.t = (2*qbar.t + t.t^2/n.t + t.t^2*rm.t/n.t)/(2*(1 + t.t^2/n.t + t.t^2*rm.t/n.t)) +
                     sqrt((2*qbar.t + t.t^2/n.t + t.t^2*rm.t/n.t)^2/(4*(1 + t.t^2/n.t + t.t^2*rm.t/n.t)^2)-
                            qbar.t^2/(1 + t.t^2/n.t + t.t^2*rm.t/n.t)),
                   
                   ci.l = qbar.d-sqrt((qbar.c-lb.c)^2+(ub.t-qbar.t)^2),
                   ci.u = qbar.d+sqrt((ub.c-qbar.c)^2+(qbar.t-lb.t)^2)
    )%>%
    dplyr::select(qbar.c, qbar.t, qbar.d, ubar.c, ubar.t,  ci.l, ci.u)%>%
    dplyr::mutate(reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))
  
  return(mice.res)
  
}
