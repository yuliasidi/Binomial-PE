
#model that generates missing data
anal.miss.run <- function(df, M2, b.trt = 0, b.Y = 0, b.X1 = 0, b.X2 = 0, do = 0.1, seed = seed,
                     dt.out = FALSE, 
                     mice.anal = TRUE,
                     norm.anal = FALSE,
                     ci.method = FM.CI,
                     t.inc = TRUE,
                     seed.mice = seed.mice)
  {
  
  out<-miss.impose.x2.cont(df = df, b.trt = b.trt, b.Y = b.Y, b.X1 = b.X1, b.X2 = b.X2, do = do, seed = seed)
  
  do.arm <- out%>%
    dplyr::group_by(trt)%>%
    summarise(do = mean(is.na(y.m)))%>%
    tidyr::spread(key = trt, value = 'do')
  check.rel <-summary(glm(y.m~as.factor(trt) + x1 + x2, out, family=binomial))$coefficients[,1]
  
  if (dt.out){  
    return(out)
  }
  
  #calculate do rates per arm
  

  else{
      #CCA
      out.ci.cca <- ci.method(out, M2,'y.m')%>%
        dplyr::select(phat.d, ci.l, ci.u, reject.h0)%>%
        dplyr::mutate(strategy = 'cca')
      
      
      
      #best-case scenario
      out.ci.best <- out%>%
        dplyr::mutate(y=ifelse(is.na(y.m)=="FALSE",y.m,1))%>%
        ci.method(M2,'y')%>%
        dplyr::select(phat.d, ci.l, ci.u, reject.h0)%>%
        dplyr::mutate(strategy = 'best')
      
      
      #worst-case scenario
      out.ci.worst <- out%>%
        dplyr::mutate(y=ifelse(is.na(y.m)=="FALSE",y.m,0))%>%
        ci.method(M2, 'y')%>%
        dplyr::select(phat.d, ci.l, ci.u, reject.h0)%>%
        dplyr::mutate(strategy = 'worst')
      
      out.ci <- bind_rows(out.ci.cca, out.ci.best, out.ci.worst)
    

    #mice
  if (mice.anal){
    out.ci.mice <- out%>%
      mice.run(n.mi = num.mi, ci.method=ci.method, M2=M2, t.inc = TRUE, seed.mice = seed.mice)%>%
      dplyr::mutate(strategy = sprintf('mice m=%d',num.mi))%>%
      dplyr::rename(phat.d = qbar)%>%
      dplyr::select(phat.d, ci.l, ci.u, reject.h0, strategy)
    
    out.ci <- dplyr::bind_rows(out.ci, out.ci.mice)
  
  }
  
    #norm
    if (norm.anal){
      out.ci.norm <- out%>%
        norm.run(n.mi = num.mi, ci.method=ci.method, M2=M2)%>%
        dplyr::mutate(strategy = sprintf('norm m=%d',num.mi))%>%
        dplyr::rename(phat.d = qbar)%>%
        dplyr::select(phat.d, ci.l, ci.u, reject.h0, strategy)
      
      out.ci <- dplyr::bind_rows(out.ci, out.ci.norm)
      
      
    }  
    
    anal.return <- list(out.ci, do.arm, check.rel)%>%purrr::set_names(c("ci","do","glm"))
      
    return(anal.return)
      
  }
  
  }
  