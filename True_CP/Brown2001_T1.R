# Compare results with Brown et al 2001 
library(dplyr)
library(purrr)
library(ggplot2)

bounds_cp <- function(df){
  df%>%
    dplyr::mutate(t = qnorm(0.025)^2/n,
                  ci.u = ceiling(n*p/(1+t)+n*t/(2*(1+t))-sqrt((2*n*p+t*n)^2-4*(1+t)*(n*p)^2)/(2*(1+t))),
                  ci.l = floor(n*p/(1+t)+n*t/(2*(1+t))+sqrt((2*n*p+t*n)^2-4*(1+t)*(n*p)^2)/(2*(1+t))))
} 

cprob <- function(a,b,n,p){
  prob <- round(sum(dbinom(a:b, n, p)),3)
  return(prob)
}


################################################################

#Table 1

n.luck <- data.frame(n = c(17, 20, 25, 30, 35, 37, 42, 44, 49 ,
                           10, 12, 13, 15, 18, 23, 28, 33, 40),
                     luck = c(rep('lucky',9),rep('unlucky',9)),
                     p=0.5)%>%
  bounds_cp()%>%
  dplyr::mutate(cp = pmap(list(ci.l,ci.u,n,p), cprob),
                cp = map_dbl(cp, as.numeric))

#Results in Table 2
n.luck2 <- data.frame(n = c(592, 954, 1279, 1583, 1876),
                      p=0.005)%>%
  bounds_cp()%>%
  dplyr::mutate(cp = pmap(list(ci.l,ci.u,n, p), cprob),
                cp = map_dbl(cp, as.numeric))


#Fix n=100 and look at different p's
f3 <- data.frame(p = seq(0.01,0.99,0.001), n=200)%>%
  bounds_cp()%>%
  dplyr::mutate(cp = pmap(list(ci.l,ci.u,n, p), cprob),
                cp = map_dbl(cp, as.numeric))


f3%>%
  ggplot2::ggplot(aes(x = p, y = cp)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size=0.1) +
  ggplot2::geom_hline(yintercept=0.95, colour="red")
  

source("True_CP/CP_functions.R")

#Figure 5 (Wilson) from the article
f5.wilson <- data.frame(p = seq(0.01,0.99,0.001), n=50)
f5.wilson.cp <- bind_cols(f5.wilson,cp = pmap_dbl(f5.wilson,cp_1prop.wilson, alpha=0.025))

f5.wilson.cp%>%
  ggplot2::ggplot(aes(x = p, y = cp)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size=0.1) +
  ggplot2::geom_hline(yintercept=0.95, colour="red")
