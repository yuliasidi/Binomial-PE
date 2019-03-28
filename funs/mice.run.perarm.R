mice.run.perarm <- function(dt, n.mi = 5, method = "logreg", M2,
                     ci.method=ci.method){
  
  dt.mice.C <- dt%>%
    filter(trt=="C")%>%
    dplyr::select(y.m,X)%>%
    dplyr::mutate(y.m = as.factor(y.m))

  dt.mice.T <- dt%>%
    filter(trt=="T")%>%
    dplyr::select(y.m,X)%>%
    dplyr::mutate(y.m = as.factor(y.m))
  
  dt.mice.imp.C<- mice::mice(
    data = dt.mice.C,
    m=n.mi,
    method = method,
    seed = 9875,
    maxit = 100,
    printFlag = FALSE
  )
  
  dt.mice.imp.T<- mice::mice(
    data = dt.mice.T,
    m=n.mi,
    method = method,
    seed = 9876,
    maxit = 100,
    printFlag = FALSE
  )
  imp.n <- as.list(data.frame(i = seq(1, n.mi,1)))

  dt.mice.ci.perm <- purrr::pmap_dfr(imp.n, .f=function(i){
    dt <- mice::complete(dt.mice.imp.C,i)%>%
      dplyr::mutate(y.m = as.numeric(y.m)-1, 
                    trt = "C")%>%
      dplyr::bind_rows(
        mice::complete(dt.mice.imp.T,i)%>%
          dplyr::mutate(y.m = as.numeric(y.m)-1, 
                        trt = "T"))%>%
      ci.method(M2, 'y.m')%>%
      dplyr::select(-reject.h0)
  }, .id = "i")
  
  mice.res <- mi.comb(dt.mice.ci.perm)%>%
    dplyr::mutate(reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))
     
 
  return(mice.res)
  
}
