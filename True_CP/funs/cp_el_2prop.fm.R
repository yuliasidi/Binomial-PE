cp_el_2prop.fm <- function(n1,p1,n2,p2,M2,alpha){
  dt.bin <- data.frame(expand.grid(x = seq(0,n1,1),
                                   y = seq(0,n2,1)))
  dt.bin <- dt.bin%>%
    dplyr::mutate(x.pmf = dbinom(x,n1,p1),
           y.pmf = dbinom(y,n2,p2),
           pmf   = x.pmf * y.pmf)%>%
    dplyr::mutate(theta = n2/n1,
                  a = 1 + theta, 
                  b = -1*(1 + theta + x/n1 + theta*y/n2 + M2*(2 + theta)), 
                  c = M2^2 + M2*(2*x/n1 + theta + 1) + x/n1 + theta*y/n2,
                  d = -x/n1*M2*(1 + M2))%>%
    dplyr::mutate(v = (b/(3*a))^3 - b*c/(6*a^2) + d/(2*a),
                  u = sign(v)*sqrt((b/(3*a))^2 - c/(3*a)),
                  w = (pi+acos(v/(u^3)))/3)%>%
    dplyr::mutate(p1.rmle = 2*u*cos(w) - b/(3*a),
                  p2.rmle = p1.rmle - M2)%>%
    dplyr::select(-a, -b, -c, -d, -v, -u, -w, -theta)%>%
    mutate(ci.l = x/n1-y/n2 + 
             qnorm(alpha)*sqrt(p1.rmle*(1-p1.rmle)/n1 + p2.rmle*(1-p2.rmle)/n2),
           ci.u = x/n1-y/n2 + 
             qnorm(1-alpha)*sqrt(p1.rmle*(1-p1.rmle)/n1 + p2.rmle*(1-p2.rmle)/n2),
           p = p1-p2,
           l = (ci.u - ci.l)*pmf,
           I = ifelse(p >= ci.l & p <= ci.u,1,0),
           I.na = ifelse(is.na(I)==T,1,0))

  cp <- as.numeric(dt.bin%>%
                     filter(I==1)%>%
                     summarise(cp=sum(pmf)))
  el <- as.numeric(dt.bin%>%
                     summarise(el=sum(l, na.rm=T)))
  na.sum <- as.numeric(dt.bin%>%
                         summarise(na.sum = sum(I.na)))
  res <- list(cp,el,na.sum)%>%purrr::set_names(c("cp","el", "na.sum"))
  
  return(res)  
}


