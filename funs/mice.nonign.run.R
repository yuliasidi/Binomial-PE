mice.nonign.run <- function(dt, n.mi = 5, m.mi = 10, M2,
                     ci.method=ci.method, seed.mice, mu.C = 1, sd.C = 0, mu.T = 1, sd.T = 0){
  
    set_env <- function(){
      
      thisenv <- new.env()
      thisenv$mice.out.phat <- list()
      thisenv
      
    }
  
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
  
  
  set.seed(seed.mice)
  
  thisenv <- set_env()
  
  maxit <- 20
  
  #MICE for control arm
  dt.C.imp <- mice::mice(
    data = dt.C,
    m=n.mi,
    method = "logreg.p",
    predictorMatrix=predM.C,
    seed = seed.mice,
    maxit = maxit,
    printFlag = FALSE
  )
  
  thisenv$mice.out.phat[seq(maxit,length(thisenv$mice.out.phat),maxit)]
  
  
  imp.n <- as.list(data.frame(i = seq(1,n.mi,1)))
  
  dt.mice.p <- purrr::pmap_dfr(imp.n, .f=function(i){
  
  })
    
  thisenv <- set_env()
  
  #MICE for the new teratment
  dt.T.imp <- mice::mice(
    data = dt.T,
    m=n.mi,
    method = "logreg.p",
    predictorMatrix=predM.T,
    seed = seed.mice,
    maxit = 20,
    printFlag = FALSE
  )
  
  
  dt.mice.p <- purrr::pmap_dfr(imp.n, .f=function(i){
    dtim <- 
      bind_rows(mice::complete(dt.C.imp,i)%>%
                  dplyr::mutate(y.m = as.numeric(y.m)-1,
                                trt = 'C'),
                mice::complete(dt.T.imp,i)%>%
                  dplyr::mutate(y.m = as.numeric(y.m)-1,
                                trt = 'T'))%>%
      ci.method(M2, 'y.m')%>%
      dplyr::select(-reject.h0, -1)
  }, .id = "n")
  k.mults <- tibble(m = seq(1, m.mi,1),
                    k.C = rnorm(m.mi, mu.C, sd.C),
                    k.T = rnorm(m.mi, mu.T, sd.T))
  
  nested.mi.res <- k.mults%>%
    dplyr::mutate(mi.ci = purrr::map2(k.C, k.T, .f = function(k.C, k.T){
      
      #k-multiplier for control arm
      assign("k.mult", k.C, envir = .GlobalEnv)
      

      #k-multoplie for the new treatment
      assign("k.mult", k.T, envir = .GlobalEnv)
      

      
      imp.n <- as.list(data.frame(i = seq(1,n.mi,1)))
      
      dt.mice.ci.perm <- purrr::pmap_dfr(imp.n, .f=function(i){
        dtim <- 
          bind_rows(mice::complete(dt.C.imp,i)%>%
                      dplyr::mutate(y.m = as.numeric(y.m)-1,
                                    trt = 'C'),
                    mice::complete(dt.T.imp,i)%>%
                      dplyr::mutate(y.m = as.numeric(y.m)-1,
                                    trt = 'T'))%>%
          ci.method(M2, 'y.m')%>%
          dplyr::select(-reject.h0, -1)
      }, .id = "n")
      
      return(dt.mice.ci.perm)
      
      remove(k.mult)
  }))
  
 
 mice.res <- nested.mi.res%>%
    unnest()%>%
   nested.mi.comb()%>%
   dplyr::mutate(reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))
    
  return(mice.res)
  
}
