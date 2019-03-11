wald.ci <- function(df, M2, y){
  df%>%
    group_by(trt)%>%
    summarise(phat = mean(!!rlang::sym(y), na.rm = T), n = sum(!is.na(!!rlang::sym(y))))%>%
    recast(1 ~ trt + variable, measure.var = c("phat",'n'))%>%
    mutate(phat.d = C_phat-T_phat, 
           var.d = C_phat*(1-C_phat)/(C_n) + T_phat*(1-T_phat)/(T_n),
           ci.l = C_phat-T_phat - qnorm(1-alpha)*sqrt(var.d),
           ci.u = C_phat-T_phat + qnorm(1-alpha)*sqrt(var.d),
           reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))
}

