norm.run <- function(dt, n.mi = 5, M2,
                     ci.method=ci.method){
 
  rngseed(98765)                                                   
  data1 <- as.matrix(dt%>%dplyr::select(-c(pat_id, trt)))
  prelim.mi <- prelim.norm(data1)
  theta.init <- em.norm(prelim.mi,showits=FALSE)
  theta.hat <- makeparam.norm(prelim.mi,theta.init)
  
  
  # theta <- da.norm(prelim.mi, theta.init, steps = 20, showits = TRUE)
  # #Data augmentation
  
  da.list <- list()
  
  for (i in 1:1000){
    theta.hat <- da.norm(prelim.mi, theta.hat, steps=1)
  #  da.list[[i]] <- getparam.norm(prelim.mi, theta.hat)
  }
  
  # y.mu<-list()
  # for (i in 1:1000){y.mu[[i]] <- da.list[[i]][[1]][3]}
  
  
  imp.n <- as.list(data.frame(i = seq(1,n.mi,1)))
  
  
  imp.ci.perm <- purrr::pmap_dfr(imp.n, .f=function(i){
    
    imp.dat <- imp.norm(prelim.mi, theta.hat, data1)
    imp.dat <- as_tibble(imp.dat)%>%
      bind_cols(dt%>%
                  dplyr::mutate(r = ifelse(is.na(y.m)==T,1,0))%>%
                  dplyr::select(pat_id,r,trt))
    
    omega <- mean(imp.dat$y.m)
    thresh <- omega - qnorm(omega)*sqrt(omega*(1-omega))
    
    imp.res<-imp.dat%>%
      dplyr::mutate(y.round = ifelse(y.m>thresh,1,0),
                    y.imp = ifelse(r==1,y.round, y.m))%>%
      dplyr::select(-c(y.m,y.round, r))%>%
      ci.method(M2, 'y.imp')%>%
      dplyr::select(-reject.h0)}, 
    
    .id = "i")
  
  norm.res <- mi.comb(imp.ci.perm)%>%
    dplyr::mutate(reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))
  
  #norm.return <- list(norm.res, y.mu)%>%purrr::set_names(c("final_ci","param"))
  
  return(norm.res)
  
}
  