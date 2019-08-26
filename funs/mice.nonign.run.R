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
  
  thisenv <<- set_env()
  
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
  
  phats <- thisenv$mice.out.phat[seq(maxit,length(thisenv$mice.out.phat),maxit)]
  
  imp.n <- tibble(n = seq(1,n.mi,1))
  
  #add the imputed phats per subject to the original incomplete data
  dt.C.phat <- imp.n%>%
    dplyr::mutate(dt.phat = map(n, .f = function(n){
      dt.C%>%
        dplyr::mutate(wy = seq(1,n(),1),
                      trt = "C")%>%
        dplyr::left_join(phats[[n]]%>%
                           dplyr::mutate(p.thresh = runif(n())), by = 'wy')
    }))

  thisenv <<- set_env()
  
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
  
  phats <- thisenv$mice.out.phat[seq(maxit,length(thisenv$mice.out.phat),maxit)]
  
  #add the imputed phats per subject to the original incomplete data
  dt.T.phat <- imp.n%>%
    dplyr::mutate(dt.phat = map(n, .f = function(n){
      dt.T%>%
        dplyr::mutate(wy = seq(1,n(),1),
                      trt = "T")%>%
        dplyr::left_join(phats[[n]]%>%
                           dplyr::mutate(p.thresh = runif(n())), by = 'wy')
    }))
  
 
  set.seed(seed.mice)

  k.mults <- tibble(m = seq(1, m.mi,1),
                    k.C = rnorm(m.mi, mu.C, sd.C),
                    k.T = rnorm(m.mi, mu.T, sd.T))
  
  
  # multiply phats for each arm separetly, set 0/1 for incomplete outcomes and combine imputed perrr arm data
  nested.mi.res <-
    k.mults%>%
    dplyr::mutate(res = purrr::map2(k.C, k.T, .f = function(k.C, k.T){
      
      dt.C <-
        dt.C.phat%>%
        dplyr::mutate(dt.new = purrr::map(dt.phat, .f = function(df){
          df%>%
            dplyr::mutate(p.new = k.C * p)%>%
            dplyr::mutate(y.k = ifelse(p.new >= p.thresh,1,0),
                          y.m = ifelse(is.na(y.m) == T, y.k, as.numeric(y.m) - 1))%>%
            dplyr::select(pat_id, x1, x2, y.m, trt)
          
        }))%>%
        dplyr::select(-dt.phat)
      
      dt.T <- 
      dt.T.phat%>%
        dplyr::mutate(dt.new = purrr::map(dt.phat, .f = function(df){
          df%>%
            dplyr::mutate(p.new = k.T * p)%>%
            dplyr::mutate(y.k = ifelse(p.new >= p.thresh,1,0),
                          y.m = ifelse(is.na(y.m) == T, y.k, as.numeric(y.m) - 1))%>%
            dplyr::select(pat_id, x1, x2, y.m, trt)
              
       }))%>%
        dplyr::select(-dt.phat)
      
      
      dt.all <-
        bind_rows(dt.C%>%tidyr::unnest(),
                  dt.T%>%tidyr::unnest())%>%
        tidyr::nest(-n)%>%
        dplyr::mutate(ci.out = purrr::map(data, ci.method, M2 = M2, y = "y.m"))%>%
        dplyr::select(-data)%>%
        tidyr::unnest()#%>%
        #dplyr::select(n, phat.d, var.d)
  
                
  }))%>%
    tidyr::unnest()
  

 if(method!='wn') 
   {
   mice.res <- nested.mi.res%>%
     dplyr::select(m, k.C, k.T, n, phat.d, var.d)%>%
     nested.mi.comb()%>%
     dplyr::mutate(reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))  
    
   }
  
  if(method=='wn') 
  {
    
      mice.res <- nested.mi.res%>%
        nested.mi.comb.wn()%>%
        dplyr::mutate(reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))    
    
    if(pro_wnmi){
      mice.res <- nested.mi.res%>%
        proper_wnmi()%>%
        dplyr::mutate(reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))    
    }
    
  }
    
  return(mice.res)
  
}
