dt.sim.x2.cont.inter <- function(p_C, p_T, n.arm, mu1 ,mu2, sigma1, sigma2, r12, b1, b2t, b2c){
  
  xcov <- matrix(c(sigma1^2, sigma1*sigma2*r12, sigma1*sigma2*r12, sigma2^2), nrow = 2, ncol = 2)
  
  bt <- log(p_T*(1-p_C)/((1-p_T)*p_C))
  
  b0.c <- log(p_C/(1-p_C)) - b1*mu1 - b2c*mu2
  b0.t <- log(p_T/(1-p_T)) - b1*mu1 - b2t*mu2 - bt
  
  x.T <- mvrnorm(n = n.arm, mu = c(mu1,mu2), Sigma = xcov)
  x.C <- mvrnorm(n = n.arm, mu = c(mu1,mu2), Sigma = xcov)
  
  dt <- tibble(x1 = x.T[,1], x2 = x.T[,2], trt = "T",
               p.y = 1/(1+exp(-b0.t - b1*x1 - b2t*x2 - bt)))%>%
    dplyr::bind_rows(tibble(x1 = x.C[,1], x2 = x.C[,2], trt = "C",
                            p.y = 1/(1+exp(-b0.c - b1*x1 - b2c*x2))))%>%
    dplyr::mutate(trtn = case_when(trt=='T' ~ 1, 
                                   TRUE ~ 0))%>%
    dplyr::mutate(p.thresh = runif(n()))%>%
    dplyr::mutate(y = as.numeric(p.y >= p.thresh),
                  pat_id = seq(1,n(),1))%>%
    dplyr::select(pat_id, trt, trtn, y, x1, x2)
  
  return(dt)
  
}
