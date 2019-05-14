
#model that generates missing data and analyzes it by CCA/best/worse or nested MI strategy
anal.miss.run <- function(df, M2, b.trt = 0, b.Y = 0, b.X1 = 0, b.X2 = 0, b.ty = 0, do = 0.1, seed = seed,
                     dt.out = FALSE, 
                     sing.anal = TRUE,
                     mice.anal = FALSE,
                     bw.anal = FALSE,
                     mu.C =  1, sd.C = 0,
                     mu.T =  1, sd.T = 0,
                     ci.method = FM.CI,
                     seed.mice = seed.mice)
  {
  
  out<-miss.impose.x2.cont(df = df, b.trt = b.trt, b.Y = b.Y, b.X1 = b.X1, b.X2 = b.X2, b.ty = b.ty, do = do, seed = seed)
  
  do.arm <- out%>%
    dplyr::group_by(trt)%>%
    summarise(do = mean(is.na(y.m)))%>%
    tidyr::spread(key = trt, value = 'do')
  
  check.rel <-summary(glm(y.m~as.factor(trt) + x1 + x2, out, family=binomial))$coefficients[,1]
  
  if (dt.out){  
    return(out)
  }

  else{
    if (sing.anal){
      #CCA
      out.ci.cca <- ci.method(out, M2,'y.m')%>%
        dplyr::select(C_phat, T_phat, phat.d, ci.l, ci.u, reject.h0)%>%
        dplyr::mutate(strategy = 'cca')
      
      
      
      #best-case scenario
      out.ci.best <- out%>%
        dplyr::mutate(y=ifelse(is.na(y.m)=="FALSE",y.m,1))%>%
        ci.method(M2,'y')%>%
        dplyr::select(C_phat, T_phat, phat.d, ci.l, ci.u, reject.h0)%>%
        dplyr::mutate(strategy = 'best')
      
      
      #worst-case scenario
      out.ci.worst <- out%>%
        dplyr::mutate(y=ifelse(is.na(y.m)=="FALSE",y.m,0))%>%
        ci.method(M2, 'y')%>%
        dplyr::select(C_phat, T_phat, phat.d, ci.l, ci.u, reject.h0)%>%
        dplyr::mutate(strategy = 'worst')
      
      out.ci <- bind_rows(out.ci.cca, out.ci.best, out.ci.worst)
      
    }
          

    #mice
  if (mice.anal){
    
    out.ci <- out%>%
      mice.nonign.run(n.mi = num.n.mi, m.mi = num.m.mi, ci.method=ci.method, M2=M2, seed.mice = seed.mice,
                      mu.C =  mu.C, sd.C = sd.C,
                      mu.T =  mu.T, sd.T = sd.T)%>%
      dplyr::mutate(strategy = "nested mice",
                    k.C.spec = sprintf("normal(%s, %s)", mu.C, sd.C),
                    k.T.spec = sprintf("normal(%s, %s)", mu.T, sd.T))
  }
    
    #bw for mnars only
    if (bw.anal){
      
      out.ci <- out%>%
        dplyr::mutate(y=ifelse(is.na(y.m)=="FALSE",y.m,
                               ifelse(trt=="C",1,0)))%>%
        ci.method(M2,'y')%>%
        dplyr::select(C_phat, T_phat, phat.d, ci.l, ci.u, reject.h0)%>%
        dplyr::mutate(strategy = 'bw')
    }
    
    
    
    anal.return <- list(out.ci, do.arm, check.rel)%>%purrr::set_names(c("ci","do","glm"))
    
    return(anal.return)
    
  }    
      
}
  

  