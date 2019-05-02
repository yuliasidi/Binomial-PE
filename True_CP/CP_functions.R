#calculate true coverage probability for 1 Binomial proportion derived from Wald CI 
cp_1prop.wald <- function(n,p,alpha){
  dt.bin <- data.frame(x = seq(0,n,1))
  dt.bin <- dt.bin%>%
    mutate(pmf = dbinom(x,n,p),
           ci.l = x/n + qnorm(alpha)*sqrt(x/n*(1-x/n)/n),
           ci.u = x/n + qnorm(1-alpha)*sqrt(x/n*(1-x/n)/n),
           I = ifelse(p >= ci.l & p <= ci.u,1,0))
  cp <- as.numeric(dt.bin%>%
                     filter(I==1)%>%
                     summarise(cp=sum(pmf)))
  return(cp)  
}

#calculate true coverage probability for 1 Binomial proportion derived from Wilson CI 
cp_1prop.wilson <- function(n,p,alpha){
  t <- qnorm(1-alpha)^2/n
  dt.bin <- data.frame(x = seq(0,n,1))
  dt.bin <- dt.bin%>%
    mutate(pmf = dbinom(x,n,p),
           ci.l = (x/n + t/2)/(1+t) - sqrt(t^2/4+t*x/n*(1-x/n))/(1+t),
           ci.u = (x/n + t/2)/(1+t) + sqrt(t^2/4+t*x/n*(1-x/n))/(1+t),
           I = ifelse(p >= ci.l & p <= ci.u,1,0))
  cp <- as.numeric(dt.bin%>%
                     filter(I==1)%>%
                     summarise(cp=sum(pmf)))
  return(cp)  
}

#calculate true coverage probability for 2 Binomial proportions diff derived from Wald CI 
cp_2prop.wald <- function(n1,p1,n2,p2,alpha){
  dt.bin <- data.frame(expand.grid(x = seq(0,n1,1),
                                   y = seq(0,n2,1)))
  dt.bin <- dt.bin%>%
    mutate(x.pmf = dbinom(x,n1,p1),
           y.pmf = dbinom(y,n2,p2),
           pmf   = x.pmf * y.pmf,
           ci.l = x/n1-y/n2 + qnorm(alpha)*sqrt(x/n1*(1-x/n1)/n1 + y/n2*(1-y/n2)/n2),
           ci.u = x/n1-y/n2 + qnorm(1-alpha)*sqrt(x/n1*(1-x/n1)/n1 + y/n2*(1-y/n2)/n2),
           p = p1-p2,
           I = ifelse(p >= ci.l & p <= ci.u,1,0))
  cp <- as.numeric(dt.bin%>%
                     filter(I==1)%>%
                     summarise(cp=sum(pmf)))
  return(cp)  
}

#calculate true coverage probability for 2 Binomial proportions diff derived from Wilson CI 
cp_2prop.wilson <- function(n1,p1,n2,p2,alpha){
  dt.bin <- data.frame(expand.grid(x = seq(0,n1,1),
                                   y = seq(0,n2,1)))
  dt.bin <- dt.bin%>%
    mutate(x.pmf = dbinom(x,n1,p1),
           y.pmf = dbinom(y,n2,p2),
           pmf   = x.pmf * y.pmf,
           z = qnorm(1-alpha),
                  l.C = (x/n1 + z^2/(2*n1) - 
                           z*sqrt((x/n1*(1-x/n1)+z^2/(4*n1))/n1))/(1+z^2/n1),
                  u.C = (x/n1 + z^2/(2*n1) + 
                           z*sqrt((x/n1*(1-x/n1)+z^2/(4*n1))/n1))/(1+z^2/n1),
                  
                  l.T = (y/n2 + z^2/(2*n2) - 
                           z*sqrt((y/n2*(1-y/n2)+z^2/(4*n2))/n2))/(1+z^2/n2),
                  u.T = (y/n2 + z^2/(2*n2) + 
                           z*sqrt((y/n2*(1-y/n2)+z^2/(4*n2))/n2))/(1+z^2/n2),
                  
                  ci.l = x/n1-y/n2-sqrt((x/n1-l.C)^2+(u.T-y/n2)^2),
                  ci.u = x/n1-y/n2+sqrt((u.C-x/n1)^2+(y/n2-l.T)^2),
           p = p1-p2,
           I = ifelse(p >= ci.l & p <= ci.u,1,0))
  cp <- as.numeric(dt.bin%>%
                     filter(I==1)%>%
                     summarise(cp=sum(pmf)))
  return(cp)  
}


########################
## Expected CI Length ##
########################

#expected CI length for 2 Binomial proportions diff derived from Wald CI 
el_2prop.wald <- function(n1,p1,n2,p2,alpha){
  dt.bin <- data.frame(expand.grid(x = seq(0,n1,1),
                                   y = seq(0,n2,1)))
  dt.bin <- dt.bin%>%
    mutate(x.pmf = dbinom(x,n1,p1),
           y.pmf = dbinom(y,n2,p2),
           pmf   = x.pmf * y.pmf,
           ci.l = x/n1-y/n2 + qnorm(alpha)*sqrt(x/n1*(1-x/n1)/n1 + y/n2*(1-y/n2)/n2),
           ci.u = x/n1-y/n2 + qnorm(1-alpha)*sqrt(x/n1*(1-x/n1)/n1 + y/n2*(1-y/n2)/n2),
           l = (ci.u - ci.l)*pmf)
  el <- as.numeric(dt.bin%>%
                     summarise(el=sum(l)))
  return(el)  
}



#expected CI length 2 Binomial proportions diff derived from Wilson CI 
el_2prop.wilson <- function(n1,p1,n2,p2,alpha){
  dt.bin <- data.frame(expand.grid(x = seq(0,n1,1),
                                   y = seq(0,n2,1)))
  dt.bin <- dt.bin%>%
    mutate(x.pmf = dbinom(x,n1,p1),
           y.pmf = dbinom(y,n2,p2),
           pmf   = x.pmf * y.pmf,
           z = qnorm(1-alpha),
           l.C = (x/n1 + z^2/(2*n1) - 
                    z*sqrt((x/n1*(1-x/n1)+z^2/(4*n1))/n1))/(1+z^2/n1),
           u.C = (x/n1 + z^2/(2*n1) + 
                    z*sqrt((x/n1*(1-x/n1)+z^2/(4*n1))/n1))/(1+z^2/n1),
           
           l.T = (y/n2 + z^2/(2*n2) - 
                    z*sqrt((y/n2*(1-y/n2)+z^2/(4*n2))/n2))/(1+z^2/n2),
           u.T = (y/n2 + z^2/(2*n2) + 
                    z*sqrt((y/n2*(1-y/n2)+z^2/(4*n2))/n2))/(1+z^2/n2),
           
           ci.l = x/n1-y/n2-sqrt((x/n1-l.C)^2+(u.T-y/n2)^2),
           ci.u = x/n1-y/n2+sqrt((u.C-x/n1)^2+(y/n2-l.T)^2),
           l = (ci.u - ci.l)*pmf)
  el <- as.numeric(dt.bin%>%
                     summarise(el=sum(l)))
  return(el)  
}
