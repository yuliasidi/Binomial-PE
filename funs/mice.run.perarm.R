mice.run.perarm <- function(dt, n.mi = 5, method = "logreg", M2,
                     ci.method=ci.method, seed.mice){

  dt.C <- dt%>%
    dplyr::filter(trt=="C")%>%
    dplyr::select(pat_id, x1, x2, y.m)%>%
    dplyr::mutate(y.m = as.factor(y.m))  
  
  dt.T <- dt%>%
    dplyr::filter(trt=="T")%>%
    dplyr::select(pat_id, x1, x2, y.m)%>%
    dplyr::mutate(y.m = as.factor(y.m))  
  
  predM.C <- mice::make.predictorMatrix(data=dt.C)
  predM.C[, "pat_id"] <- 0
  
  predM.T <- mice::make.predictorMatrix(data=dt.T)
  predM.T[, "pat_id"] <- 0
  
  maxit <- 20
  
  dt.C.imp <- mice::mice(
    data = dt.C,
    m=n.mi,
    method = "logreg",
    predictorMatrix=predM.C,
    seed = seed.mice,
    maxit = maxit,
    printFlag = FALSE
  )
  
  dt.T.imp <- mice::mice(
    data = dt.T,
    m=n.mi,
    method = "logreg",
    predictorMatrix=predM.T,
    seed = seed.mice,
    maxit = 20,
    printFlag = FALSE
  )
  
  imp.n <- seq(1,n.mi,1)
  
  dt.mice.ci.perm <- purrr::map_dfr(imp.n, .f=function(i){
    dt <- mice::complete(dt.C.imp ,i)%>%
      dplyr::mutate(trt = "C")%>%
      dplyr::bind_rows(
        mice::complete(dt.T.imp ,i)%>%
          dplyr::mutate(trt = "T"))%>%
      dplyr::mutate(y.m = as.numeric(y.m)-1)%>%
      ci.method(M2, 'y.m')%>%
      dplyr::select(-reject.h0)
  }, .id = "i")
  
  mice.res <- mi.comb(dt.mice.ci.perm)%>%
    dplyr::mutate(reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))
  
  return(mice.res)
  
}
