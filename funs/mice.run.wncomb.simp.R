mice.run.wncomb.simp <- function(dt, n.mi = 5, method = "logreg", M2){
  
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
      dplyr::rename(qhat.c = C, qhat.t = T)
  }, .id = "i")
  
  mice.res <- dttmp%>%
    dplyr::summarise(qbar.c = mean(qhat.c), qbar.t = mean(qhat.t))%>%
    dplyr::mutate(qbar.d = qbar.c - qbar.t,
                  n.c = nrow(complete(dt.mice.imp,1)%>%filter(trt=="C")),
                  n.t = nrow(complete(dt.mice.imp,1)%>%filter(trt=="T")),
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
    dplyr::select(qbar.c, qbar.t, qbar.d, ci.l, ci.u)%>%
    dplyr::mutate(reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))
  
     
 
  return(mice.res)
  
}
