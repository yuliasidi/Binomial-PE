
source("Step_0_init.R")
source("funs/ss.wald.R")
source("funs/biv.function.R")
source("funs/p.rmle.fm.R")

M2 <- c(0.05, 0.075, 0.1, 0.15, 0.2)
p_C <- seq(0.6, 0.95, 0.05)

ass.for.ss <- as.data.frame(expand.grid(p_C, M2+0, alpha, 1-beta))%>%
  rename(p_C = Var1, M2 = Var2, alpha = Var3, power = Var4 )%>%
  mutate(p_T = p_C, q_C = 1-p_C, d=round(M2-q_C,2))%>%
  mutate(ex = ifelse(d>0,"Y","N"))%>% #Exlcude scenarios where the qc will be > X2
  mutate(M2.new = ifelse(d==0,M2/2,M2))%>%
  filter(ex == "N")%>%
  select(-d, -ex, -M2)%>%
  rename(M2 = M2.new)

ass.for.ss <- unique(ass.for.ss)%>%
  dplyr::mutate(scenario.id = seq(1,n(),1))%>%
  tibble::as_tibble()%>%
  dplyr::select(-q_C)

set.seed(2222)  

ss <- ass.for.ss%>%
  mutate(n.arm = pmap_dbl(list(p_C = p_C, p_T = p_T, M2 = M2, alpha = alpha, power = power), 
                           .f=ss.wald))%>%
  mutate(method = 'wald')%>%
  dplyr::mutate(bounds = pmap(list(p_T = p_T, M2 = M2), 
                              .f = function(p_T, M2){
                                biv.function(rbinom(1000000, 1, p_T + M2/2), 
                                             rnorm(1000000,4,1), 
                                             desired.cor=0.3, 
                                             pearson=F,
                                             spearman=T) 
                              }))%>%
  dplyr::mutate(lb = map_dbl(bounds, .f = function(df) df$bounds[1]),
                ub = map_dbl(bounds, .f = function(df) df$bounds[2]))%>%
  dplyr::select(-bounds)


  ss.bounds <- ss%>%dplyr::bind_rows(
    ass.for.ss%>%
      p.rmle.fm()%>%
      mutate(n.arm = pmap_dbl(list(p_C = p_C, p_T = p_T, M2 = M2, p_C.rmle = p_C.rmle, p_T.rmle = p_T.rmle,
                                   alpha = alpha, power = power), 
                              .f=ss.fm))%>%
      dplyr::select(-c(p_C.rmle, p_T.rmle))%>%
      mutate(method = 'fm')%>%
      left_join(ss%>%
                  dplyr::select(scenario.id, lb, ub), by = "scenario.id")
    
  )



saveRDS(ss.bounds, "cluster/ss.bounds.rds")



######
ss.bounds<-readRDS("cluster/ss.bounds.rds")

ss.wald<-ss.bounds%>%filter(method=="wald")

SS_Wald_FM_WN_power90 <- readRDS("~/Desktop/Dissertation/Binomial PE/Binomial_PE_Progs/Old Outputs/SS_Wald_FM_WN_power90.rds")

wn.ss <- SS_Wald_FM_WN_power90%>%
  mutate(n.arm = N.total.WN/2)%>%
  select(scenario.id, n.arm)

wn.ss1<-left_join(ss.wald%>%select(-c(n.arm,lb,ub)), wn.ss, by = "scenario.id")%>%
  mutate(method="wn")%>%
  dplyr::mutate(bounds = pmap(list(p_T = p_T, M2 = M2), 
                              .f = function(p_T, M2){
                                biv.function(rbinom(1000000, 1, p_T + M2/2), 
                                             rnorm(1000000,4,1), 
                                             desired.cor=0.3, 
                                             pearson=F,
                                             spearman=T) 
                              }))%>%
  dplyr::mutate(lb = map_dbl(bounds, .f = function(df) df$bounds[1]),
                ub = map_dbl(bounds, .f = function(df) df$bounds[2]))%>%
  dplyr::select(-bounds)

ss.bounds1 <- bind_rows(ss.bounds,wn.ss1)

saveRDS(ss.bounds1, "cluster/ss.bounds.rds")



#########
wald.cat.C<-ss.wald%>%select(-c(lb,ub))%>%
  mutate(method="wald.cat")%>%
  dplyr::mutate(bounds = pmap(list(p_C = p_C, M2 = M2), 
                              .f = function(p_C, M2){
                                biv.function(rbinom(1000000, 1, p_C), 
                                             rbinom(1000000,1,0.5), 
                                             desired.cor=0.1, 
                                             pearson=F,
                                             spearman=T) 
                              }))%>%
  dplyr::mutate(lb.C = map_dbl(bounds, .f = function(df) df$bounds[1]),
                ub.C = map_dbl(bounds, .f = function(df) df$bounds[2]))%>%
  dplyr::select(-bounds)


wald.cat.T<-ss.wald%>%select(-c(lb,ub))%>%
  mutate(method="wald.cat")%>%
  dplyr::mutate(bounds = pmap(list(p_T = p_C - M2), 
                              .f = function(p_T){
                                biv.function(rbinom(1000000, 1, p_T), 
                                             rbinom(1000000,1,0.5), 
                                             desired.cor=0.1, 
                                             pearson=F,
                                             spearman=T) 
                              }))%>%
  dplyr::mutate(lb.T = map_dbl(bounds, .f = function(df) df$bounds[1]),
                ub.T = map_dbl(bounds, .f = function(df) df$bounds[2]))%>%
  dplyr::select(-bounds)

wald.cat <- left_join(wald.cat.C,wald.cat.T%>%
                        dplyr::select(scenario.id, lb.T, ub.T), by = "scenario.id")

saveRDS(wald.cat, "cluster/ss.boundscat.rds")

