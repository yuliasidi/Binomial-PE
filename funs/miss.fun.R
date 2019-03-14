
#model that generates missing data
miss.fun <- function(df, M2, b.trt = log(1), b.Y = log(1), b.X = log(1), do = 0.1,
                     dt.out = FALSE, 
                     sing.imp = TRUE, mice.anal = TRUE,
                     ci.method = FM.CI)
  {
  
  #generate probability to be missing P(R=1) = p for each subject
  tmp <- df%>%
    dplyr::mutate(trtn = case_when(trt=='T' ~ 1, 
                                   TRUE ~ 0))%>%
    dplyr::summarise(etrt = mean(trtn),
                     eX = mean(X)/10,
                     ey = mean(y))%>%
    dplyr::mutate(int = -log(1/do-1) - b.trt*etrt - b.Y*ey - b.X*eX)
  
  tmp1 <- df%>%
    dplyr::mutate(trtn = case_when(trt=='T' ~ 1, 
                                   TRUE ~ 0),
                  p = 1/(1+exp(-1*(tmp$int + b.trt*trtn + b.Y*y + b.X*X/10))))
  
  if (b.Y==0 & b.X==0 & b.trt==0) {
   
    set.seed(84759)
    
    out <- tmp1 %>%
      dplyr::mutate(r = sample(1:n(), replace = FALSE)) %>%
      dplyr::arrange(r) %>%
      dplyr::mutate(
        r = dplyr::if_else(r <= n() * do, 1, 0),
        y.m = dplyr::if_else(r==1,NA_integer_, y)
      )
   
    # sampl.miss <- tmp1%>%
    #   dplyr::select(pat_id, p)%>%
    #   dplyr::sample_frac(do, weight = p)%>%
    #   dplyr::mutate(r = 1)%>%
    #   dplyr::select(pat_id, r)
    # 
    # out <- dplyr::left_join(tmp1, sampl.miss, by =c('pat_id'))%>%
    #   dplyr::mutate(r = ifelse(is.na(r),0,r),
    #                 y.m = ifelse(r==1,NA_real_, y))
  }
  
  else {
    set.seed(84750)
    
     out <- tmp1%>%
      dplyr::arrange(desc(p))%>%
      dplyr::mutate(i = seq(1,length(pat_id),1))%>%
      dplyr::mutate(r = ifelse(i<do*length(pat_id),1,0))%>%
      dplyr::mutate(y.m = ifelse(r==1,NA_integer_,y))%>%
      dplyr::select(-i)

    }
  
  

  if (dt.out==TRUE){
    
    return(out)
  }

  else{
  
    if (sing.imp == TRUE){
      
    
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
      
    }
    

    #mice
  if (mice.anal==TRUE){
    out.ci.mice <- out%>%
      mice.run(n.mi = num.mi, ci.method=ci.method, M2=M2)%>%
      dplyr::mutate(strategy = sprintf('mice m=%d',num.mi))%>%
      dplyr::rename(phat.d = qbar)%>%
      dplyr::select(phat.d, ci.l, ci.u, reject.h0, strategy)
  
  }
  
    
  if (sing.imp == TRUE & mice.anal==TRUE){
      
    out.ci <- dplyr::bind_rows(out.ci.cca, out.ci.best, out.ci.worst, out.ci.mice)
  }  
 
  if (sing.imp == FALSE & mice.anal==TRUE){
      
      out.ci <- out.ci.mice
    }  

  if (sing.imp == TRUE & mice.anal==FALSE){
      
    out.ci <- dplyr::bind_rows(out.ci.cca, out.ci.best, out.ci.worst)
  }  
   
    return(out.ci)

  }
   
  }
  