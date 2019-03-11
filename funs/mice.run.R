mice.run <- function(dt, n.mi = 5, method = "logreg", M2,
                     ci.method=ci.method){
  
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

  dt.mice.ci.perm <- purrr::pmap_dfr(imp.n, .f=function(i){
    dt <- mice::complete(dt.mice.imp,i)%>%
      dplyr::mutate(y.m = as.numeric(y.m)-1)%>%
      ci.method(M2, 'y.m')%>%
      dplyr::select(-reject.h0)
  }, .id = "i")
  
  mice.res <- mi.comb(dt.mice.ci.perm)%>%
    dplyr::mutate(reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))
     
 
  return(mice.res)
  
}
