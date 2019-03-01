
#model that generates missing data
miss.fun <- function(df, b.trt = log(1), b.Y = log(1), b.X = log(1), do = 0.1,
                     method = 'pweight')
  {
  #generate probability to be missing P(R=1) = p for each subject
  tmp <- df%>%
    dplyr::mutate(trtn = case_when(trt=='T' ~ 1, 
                                   TRUE ~ 0))%>%
    dplyr::group_by(sim.id)%>%
    dplyr::summarise(etrt = mean(trtn),
                     eX = mean(X)/10,
                     ey = mean(y))%>%
    dplyr::mutate(int = -log(1/do-1) - b.trt*etrt - b.Y*ey - b.X*eX)
  
  tmp1 <- dplyr::left_join(df, tmp, by = "sim.id")%>%
    dplyr::mutate(trtn = case_when(trt=='T' ~ 1, 
                                   TRUE ~ 0),
                  p = 1/(1+exp(-1*(int + b.trt*trtn + b.Y*y + b.X*X/10))))%>%
    dplyr::select(-c(etrt, eX, ey, int, trtn))
  
  #pweight method randomly samples subjects to set missing by using p as weights
  if ((method == 'psort') & ((b.Y!=0) | (b.X!=0) | (b.trt!=0))){
   
     out <- tmp1%>%
      dplyr::arrange(desc(p))%>%
      dplyr::mutate(i = seq(1,length(pat_id),1))%>%
      dplyr::mutate(r = ifelse(i<do*length(pat_id),1,0))%>%
      dplyr::mutate(y.m = ifelse(r==1,as.numeric(NA),y))%>%
      dplyr::select(-i)
    
    }
  
  #psort method sets missing by sorting p and selecting the highest to be missing
  else {
    
    sampl.miss <- tmp1%>%
      dplyr::select(pat_id, p)%>%
      dplyr::sample_frac(do, weight = p)%>%
      dplyr::mutate(r = 1)%>%
      dplyr::select(pat_id, r)
    
    out <- dplyr::left_join(tmp1, sampl.miss, by =c('pat_id'))%>%
      dplyr::mutate(r = ifelse(is.na(r),0,r),
                    y.m = ifelse(r==1,as.numeric(NA), y))
  }
  
  return(out)
  
  }
  