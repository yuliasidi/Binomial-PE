dt.sim.x2.cont <- function(p_C, p_T, n.arm, mu1 ,mu2, sigma1, sigma2, r12, b1, b2){
  
  xcov <- matrix(c(sigma1^2, sigma1*sigma2*r12, sigma1*sigma2*r12, sigma2^2), nrow = 2, ncol = 2)
  
  bt <- log(p_T*(1-p_C)/((1-p_T)*p_C))
  
  b0 <- log(p_C/(1-p_C)) - b1*mu1 - b2*mu2
  
  x.T <- mvrnorm(n = n.arm, mu = c(mu1,mu2), Sigma = xcov)
  x.C <- mvrnorm(n = n.arm, mu = c(mu1,mu2), Sigma = xcov)
  
  dt <- tibble(x1 = x.T[,1], x2 = x.T[,2], trt = "T")%>%
    dplyr::bind_rows(tibble(x1 = x.C[,1], x2 = x.C[,2], trt = "C"))%>%
    dplyr::mutate(trtn = case_when(trt=='T' ~ 1, 
                                   TRUE ~ 0))%>%
    dplyr::mutate(p.y = 1/(1+exp(-b0 - b1*x1 - b2*x2 - bt*trtn)),
                  p.thresh = runif(n()))%>%
    dplyr::mutate(y = as.numeric(p.y >= p.thresh),
                  pat_id = seq(1,n(),1))%>%
    dplyr::select(pat_id, trt, trtn, y, x1, x2)
  
  return(dt)
  
}
