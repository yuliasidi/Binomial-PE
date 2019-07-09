library(dplyr)
library(purrr)
library(ggplot2)


alpha <- 0.025


#calculates upper bound for wald
wald.ub <- function(n, pc, pt, alpha){
  pc- pt + qnorm(1-alpha)*sqrt(pc*(1 - pc)/n + pt*(1 - pt)/n)
}

#calculates upper bound for wn
wn.ub <- function(n, pc, pt, alpha){
  z = qnorm(1-alpha)
  pc - pt + z*sqrt( 1/(n*(1 + z^2/n)^2)*
                      ( (1-z^2/n)*(pc*(1-pc) + pt*(1-pt)) 
                        + z*((1-2*pc)*sqrt(pc*(1-pc)/n + z^2/(4*n^2)) +
                               (1-2*pt)*sqrt(pt*(1-pt)/n + z^2/(4*n^2))  )
                        + z^2/n)  
  )
}


xx <- expand.grid(pc = seq(0.6, 0.9, 0.1),
                  M2 = c(0.025, 0.05, 0.075, 0.1, 0.15, 0.2),
                  n = seq(100, 1000, 5))%>%
  mutate(pt = pc - M2)

xx1 <- xx%>%
  mutate(wald.ub = pmap_dbl(list(n, pc, pt), wald.ub, alpha = alpha),
         wn.ub = pmap_dbl(list(n, pc, pt), wn.ub, alpha = alpha),
         up.diff = wald.ub - wn.ub)


ub.comp.plot <-
xx1%>%
  dplyr::mutate(flabel = sprintf('Delta:%s', M2))%>%
  ggplot(aes(x = n, y = up.diff)) +
  geom_line(aes(group = pc, color = pc)) +
  facet_wrap(~flabel, labeller = ggplot2::label_parsed) +
  labs(x = "N per arm",
       y = "Uppder bounds difference (Wald - WN)", color=latex2exp::TeX('p_C')) +
  theme_bw() +
  theme(legend.position = "bottom")



pdf("cluster/checks/ub_comparison.pdf")
ub.comp.plot
dev.off()





