library(dplyr)
library(purrr)

ss <- readRDS("cluster/ss.bounds.rds")

#t.wald <- readRDS("cluster/out/overall/full.type1.wald.rds")
#t.wn <- readRDS("cluster/out/overall/full.type1.wn.rds")
#true.cp <- readRDS("True_CP/summary/bineval.10.cp.wald.rds")

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

#adding noise to the simulation set-up in order to see how the upper bound is affected
add.noise <- expand.grid(noise = seq(-0.005, 0.005, 0.001), 
                         M2 = c(0.025, 0.05, 0.075, 0.1, 0.15, 0.2))
ss.noise <- 
  ss%>%
  select(scenario.id, p_C, M2, n.arm, method)%>%
  right_join(add.noise, by = "M2")%>%
  rename(pc = p_C)%>%
  mutate(pt = pc - M2 + noise)
  

#calculating wald's upper bound following noise addition
wald.ub.noise <- 
  ss.noise%>%
  filter(method=="wald")%>%
  mutate(wald.ub = pmap_dbl(list(n = n.arm, pc, pt), wald.ub, alpha = 0.025))%>%
  select(scenario.id, pc, pt, M2, wald.ub)

#calculating wn's upper bound following noise addition
wn.ub.noise <-
  ss.noise%>%
  filter(method=="wn")%>%
  mutate(wn.ub = pmap_dbl(list(n = n.arm, pc, pt), wn.ub, alpha = 0.025))%>%
  select(scenario.id, pc, pt, wn.ub, noise, n.arm)

#compare the upper bounds between wald and wn, if wald's upper bound is greater than wn's, it means that
#it is harder to reject the null with wald than with wn
ub.comp <- inner_join(wald.ub.noise,
                      wn.ub.noise,
                      by = c("scenario.id", "pc", "pt"))%>%
  mutate(wald.high = ifelse(wald.ub>wn.ub,1,0),
         up.diff = wald.ub - wn.ub)


library(ggplot2)

ub.comp%>%
  ggplot(aes(x = pc, y = up.diff)) +
  geom_point(aes(group = n.arm, color = n.arm)) +
  facet_wrap(~M2)











t.wald1 <- t.wald%>%
  mutate(d.check = C_phat - T_phat - M2)%>%
  left_join(ss%>%
              filter(method=="wald")%>%
              select(scenario.id, n.arm), by = "scenario.id")%>%
  dplyr::mutate(var.wald = C_phat * (1- C_phat)/n.arm + T_phat * (1- T_phat)/n.arm,
                tt.wald = pnorm(qnorm(alpha) + d.check/sqrt(var.wald)))

t.wn1 <- t.wn%>%
  mutate(d.check = C_phat - T_phat - M2)%>%
  left_join(ss%>%
              filter(method=="wn")%>%
              select(scenario.id, n.arm), by = "scenario.id")%>%
  dplyr::mutate( z = qnorm(1 - alpha),
                 u.C = (C_phat + z^2/(2*n.arm) + 
                          z*sqrt((C_phat*(1-C_phat)+z^2/(4*n.arm))/n.arm))/(1+z^2/n.arm),
                 
                 l.T = (T_phat + z^2/(2*n.arm) - 
                          z*sqrt((T_phat*(1-T_phat)+z^2/(4*n.arm))/n.arm))/(1+z^2/n.arm),
                 var.wn.ub = u.C*(1 - u.C)/n.arm + l.T*(1 - l.T)/n.arm,
                 tt.wn = pnorm(qnorm(alpha) + d.check/sqrt(var.wn.ub)))%>%
  dplyr::select(-c(z, u.C, l.T))
                

cc <- 
  left_join(t.wald1%>%
  select(scenario.id, reject.h0, tt.wald)%>%
  rename(tt.sim.wald = reject.h0),
t.wn1%>%
  select(scenario.id, reject.h0, tt.wn)%>%
  rename(tt.sim.wn = reject.h0),
by = "scenario.id")%>%
  mutate(diff.sim = tt.sim.wald - tt.sim.wn,
         diff = tt.wald - tt.wn)
               

t2 <- t1%>%
  mutate(tt.wald = pnorm(qqnorm()))

true.cp%>%filter(p1 == '0.85', p2=='0.75', n1 == 270)
true.cp%>%filter(p1 == '0.85', p2=='0.75', n1 == 260)






check.bound <- expand.grid(pc = seq(0.6, 0.95, 0.05), M2.alt =seq(0.195, 0.205, 0.001), stringsAsFactors = FALSE)%>%
   mutate(pt = pc - M2.alt, 
          M2 = 0.2)%>%
  inner_join(ss%>%
              select(p_C, M2, n.arm)%>%
              rename(pc = p_C), by = c('pc', 'M2'))%>%
  mutate(wald.ub = pmap_dbl(list(n = n.arm, pc, pt), wald.ub, alpha = 0.025),
         wn.ub   = pmap_dbl(list(n = n.arm, pc, pt), wn.ub, alpha = 0.025),
         ub.diff = wald.ub - wn.ub) #if ub.diff>0 => it is easier to reject the null with WN than with Wald


check.bound <- expand.grid(pc = seq(0.6, 0.95, 0.05), M2.alt =seq(0.095, 0.105, 0.001), stringsAsFactors = FALSE)%>%
  mutate(pt = pc - M2.alt, 
         M2 = 0.1)%>%
  inner_join(ss%>%
               select(p_C, M2, n.arm)%>%
               rename(pc = p_C), by = c('pc', 'M2'))%>%
  mutate(wald.ub = pmap_dbl(list(n = n.arm, pc, pt), wald.ub, alpha = 0.025),
         wn.ub   = pmap_dbl(list(n = n.arm, pc, pt), wn.ub, alpha = 0.025),
         ub.diff = wald.ub - wn.ub) #if ub.diff>0 => it is easier to reject the null with WN than with Wald


check.bound <- expand.grid(pc = seq(0.6, 0.95, 0.05), M2.alt =seq(0.07, 0.08, 0.001), stringsAsFactors = FALSE)%>%
  mutate(pt = pc - M2.alt, 
         M2 = 0.075)%>%
  inner_join(ss%>%
               select(p_C, M2, n.arm)%>%
               rename(pc = p_C), by = c('pc', 'M2'))%>%
  mutate(wald.ub = pmap_dbl(list(n = n.arm, pc, pt), wald.ub, alpha = 0.025),
         wn.ub   = pmap_dbl(list(n = n.arm, pc, pt), wn.ub, alpha = 0.025),
         ub.diff = wald.ub - wn.ub) #if ub.diff>0 => it is easier to reject the null with WN than with Wald


check.bound <- expand.grid(pc = seq(0.9, 0.9, 0.01), M2.alt =seq(0.045, 0.055, 0.001), stringsAsFactors = FALSE)%>%
  mutate(pt = pc - M2.alt, 
         M2 = 0.05, n.arm = 10000)%>%
  # inner_join(ss%>%
  #              select(p_C, M2, n.arm)%>%
  #              rename(pc = p_C), by = c('pc', 'M2'))%>%
  mutate(wald.ub = pmap_dbl(list(n = n.arm, pc, pt), wald.ub, alpha = 0.025),
         wn.ub   = pmap_dbl(list(n = n.arm, pc, pt), wn.ub, alpha = 0.025),
         ub.diff = wald.ub - wn.ub) #if ub.diff>0 => it is easier to reject the null with WN than with Wald




t1 <- readRDS("cluster/out/overall/full.type1.wald.rds")
t2 <- readRDS("cluster/out/overall/full.type1.wn.rds")
ss <- readRDS("cluster/ss.bounds.rds")

tt<-t1%>%left_join(ss%>%
                 filter(method=="wald")%>%
                 select(scenario.id, n.arm), by = "scenario.id")%>%
  mutate(wald.ub = pmap_dbl(list(n = n.arm, pc = C_phat, pt = T_phat), wald.ub, alpha = 0.025))%>%
  left_join(t2%>%left_join(ss%>%
                             filter(method=="wn")%>%
                             select(scenario.id, n.arm), by = "scenario.id")%>%
              mutate(wn.ub = pmap_dbl(list(n = n.arm, pc = C_phat, pt = T_phat), wn.ub, alpha = 0.025)), 
            by = "scenario.id")%>%
  mutate(diff = round(wald.ub - wn.ub,5),
         diff.rej = reject.h0.y - reject.h0.x)



cutoff <- function(pc, pt, n, alpha){
  z <- qnorm(1- alpha)
   (-3*z^2/n - (z/n)^4)*(pc*(1-pc) + pt*(1- pt)) +
    z*( sqrt(pc*(1-pc)/n +  z^2/(2*n)^2) * (1 - 2*pc) + sqrt(pt*(1-pt)/n +  z^2/(2*n)^2) * (1 - 2*pt)) +
    z^2/n
    
}

xx <- expand.grid(pc = seq(0.6, 0.95, 0.05),
                  M2 = c(0.05, 0.075, 0.1, 0.15, 0.2),
                  n = seq(100, 1000, 5))%>%
  mutate(pt = pc - M2)

xx1 <- xx%>%mutate(vv = pmap_dbl(list(pc, pt, n), cutoff, alpha = alpha))


x2 <- x1%>%
  purrr::map_df(.f=function(x) x$ci.wald,.id = 'sim')%>%
  dplyr::select(sim, ci.u)%>%
  dplyr::rename(ci.u.wald = ci.u)%>%
  left_join(x1%>%
              purrr::map_df(.f=function(x) x$ci.wn,.id = 'sim')%>%
              dplyr::select(sim, ci.u)%>%
              dplyr::rename(ci.u.wn = ci.u), by = "sim")%>%
  mutate(diff = ci.u.wald - ci.u.wn)

wn.10k <- x1%>%
  purrr::map_df(.f=function(x) x$ci.wn,.id = 'sim')%>%
  summarise_at(.vars=c('C_phat', 'T_phat', 'reject.h0'), .funs = mean)


