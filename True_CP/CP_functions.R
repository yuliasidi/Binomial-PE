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

cp_2prop.wald <- function(n1,p1,n2,p2,d=0,alpha){
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

cp_2prop.fm <- function(n1,p1,n2,p2,d=0,alpha){
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
