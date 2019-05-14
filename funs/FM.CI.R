# Produces point estimate and FM CI for difference in proportions & test H
FM.CI <- function(df, M2, y){
  
  df1 <- df%>%
    group_by(trt)%>%
    dplyr::summarise(phat = mean(!!rlang::sym(y), na.rm = T), n = sum(!is.na(!!rlang::sym(y))))%>%
    recast(1 ~ trt + variable, measure.var = c("phat",'n'))%>%
    rename(p_T = T_phat, p_C = C_phat)%>%
    p.rmle.fm(M2 = M2)%>%
    mutate(M2 = M2,
           phat.d = p_C-p_T, 
           var.d = p_C.rmle*(1 - p_C.rmle)/C_n + p_T.rmle*(1 - p_T.rmle)/T_n,
           ci.l = phat.d - qnorm(1-alpha)*sqrt(var.d),
           ci.u = phat.d + qnorm(1-alpha)*sqrt(var.d),
           reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))%>%
    dplyr::rename(T_phat = p_T, C_phat = p_C)
  
  return(df1)
  
}
