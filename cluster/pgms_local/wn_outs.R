library(dplyr)
library(tidyr)
library(purrr)

source("cluster/pgms/init.R")
source("funs/full.check.R")
source("funs/do.check.R")
source("funs/h0.sing.sum.R")
source("funs/h0.mice.sum.wn.R")
source("funs/plot.type1.R")
source("funs/plot.bias.R")
source("funs/plot.power.R")
source("funs/miss.desc.R")
source("funs/missing.desc.adj.R")

ss <- readRDS("cluster/ss.bounds.rds")
ss <- ss%>%
  dplyr::filter(method == "wn")

x1.sc26.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc26_do20_param1.rds")
x1.sc25.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc25_do20_param1.rds")
x1.sc23.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc23_do20_param1.rds")
x1.sc21.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc21_do20_param1.rds")
x1.sc17.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc17_do20_param1.rds")
x1.sc19.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc19_do20_param1.rds")
x1.sc2.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc2_do20_param1.rds")
x1.sc4.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc4_do20_param1.rds")
x1.sc6.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc6_do20_param1.rds")
x1.sc21.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc21_do20_param1.rds")


#additions
x1.sc27.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc27_do20_param1.rds")
x1.sc22.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc22_do20_param1.rds")
x1.sc18.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc18_do20_param1.rds")
x1.sc16.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc16_do20_param1.rds")
x1.sc15.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc15_do20_param1.rds")
x1.sc14.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc14_do20_param1.rds")
x1.sc13.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc13_do20_param1.rds")
x1.sc12.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc12_do20_param1.rds")
x1.sc11.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc11_do20_param1.rds")
x1.sc1.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc1_do20_param1.rds")

full.check(x1.sc15.sing, 15)
full.check(x1.sc14.sing, 14)
full.check(x1.sc13.sing, 13)
full.check(x1.sc12.sing, 12)
full.check(x1.sc11.sing, 11)
full.check(x1.sc1.sing, 1)

#check p_C, p_T and type1/power for full data
full.type1<-
  bind_rows(
    full.check(x1.sc21.sing, 21),
    full.check(x1.sc19.sing, 19),
    full.check(x1.sc17.sing, 17),
    full.check(x1.sc6.sing, 6),
    full.check(x1.sc4.sing, 4),
    full.check(x1.sc2.sing, 2),
    full.check(x1.sc23.sing, 23),
    full.check(x1.sc25.sing, 25),
    full.check(x1.sc26.sing, 26)
  )%>%
  dplyr::mutate(method = "wn")

saveRDS(full.type1, "cluster/out/overall/full.type1.wn.rds")

#check do rates
do.check<-
  bind_rows(do.check(x1.sc21.sing),
            do.check(x1.sc19.sing),
            do.check(x1.sc17.sing),
            do.check(x1.sc6.sing),
            do.check(x1.sc4.sing),
            do.check(x1.sc2.sing),
            do.check(x1.sc23.sing),
            do.check(x1.sc25.sing),
            do.check(x1.sc26.sing)
  )%>%
  dplyr::mutate(method = "wn", do = 0.2)

saveRDS(do.check, "cluster/out/overall/do.check.wn.20.rds")

h0.sing <-
  bind_rows(
    h0.sing.sum(x1.sc26.sing),
    h0.sing.sum(x1.sc25.sing),
    h0.sing.sum(x1.sc23.sing),
    h0.sing.sum(x1.sc21.sing),
    h0.sing.sum(x1.sc19.sing),
    h0.sing.sum(x1.sc17.sing),
    h0.sing.sum(x1.sc6.sing),
    h0.sing.sum(x1.sc4.sing),
    h0.sing.sum(x1.sc2.sing)
    
  )%>%
  dplyr::mutate(method = "wn")

saveRDS(h0.sing, "cluster/out/overall/h0.sing.wn.20.rds")


#Read and summarise results for MICE
x1.sc2.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc2_do20_param1.rds")
x1.sc4.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc4_do20_param1.rds")

x1.sc6.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc6_do20_param1.rds")
x1.sc19.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc19_do20_param1.rds")
x1.sc21.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc21_do20_param1.rds")



x1.sc17.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc17_do20_param1.rds")
x1.sc26.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc26_do20_param1.rds")
x1.sc23.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc23_do20_param1.rds")
x1.sc25.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc25_do20_param1.rds")



h0.mice <-
  bind_rows(
    h0.mice.sum.wn(x1.sc21.mice)%>%
      dplyr::filter(k.C.spec!="normal(1.35, 0.05)"),
    h0.mice.sum.wn(x1.sc2.mice),
    h0.mice.sum.wn(x1.sc4.mice),
    h0.mice.sum.wn(x1.sc6.mice),
    h0.mice.sum.wn(x1.sc19.mice),
    h0.mice.sum.wn(x1.sc17.mice),
    h0.mice.sum.wn(x1.sc26.mice),
    h0.mice.sum.wn(x1.sc23.mice),
    h0.mice.sum.wn(x1.sc25.mice)
    
    
    
    
  )%>%
  dplyr::mutate(method = "wn", N = num.n.mi, M = num.m.mi)

saveRDS(h0.mice, "cluster/out/overall/h0.mice.wn.20.rds")


##################
#### do = 15% ####
##################

x1.sc21.sing.do15 <- readRDS("cluster/out/wn/2xcont/do15/cont2xH0_wn_sing_sc21_do15_param1.rds")
x1.sc19.sing.do15 <- readRDS("cluster/out/wn/2xcont/do15/cont2xH0_wn_sing_sc19_do15_param1.rds")
x1.sc17.sing.do15 <- readRDS("cluster/out/wn/2xcont/do15/cont2xH0_wn_sing_sc17_do15_param1.rds")
x1.sc2.sing.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_sing_sc2_do15_param1.rds")
x1.sc4.sing.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_sing_sc4_do15_param1.rds")
x1.sc6.sing.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_sing_sc6_do15_param1.rds")
x1.sc23.sing.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_sing_sc23_do15_param1.rds")
x1.sc25.sing.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_sing_sc25_do15_param1.rds")
x1.sc26.sing.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_sing_sc26_do15_param1.rds")

full.type1.do15 <- 
  bind_rows(
    full.check(x1.sc21.sing.do15, 21),
    full.check(x1.sc19.sing.do15, 19),
    full.check(x1.sc17.sing.do15, 17),
    full.check(x1.sc2.sing.do15, 2),
    full.check(x1.sc4.sing.do15, 4),
    full.check(x1.sc6.sing.do15, 6),
    full.check(x1.sc23.sing.do15, 23),
    full.check(x1.sc25.sing.do15, 25),
    full.check(x1.sc26.sing.do15, 26)
  )%>%
  dplyr::mutate(method = "wald")

full.type1 <- readRDS("cluster/out/overall/full.type1.wald.rds")

#compare full datasets between simulations for do=20% and do=15%, should be exactly the same
compare::compare(full.type1%>%arrange(scenario.id), full.type1.do15%>%arrange(scenario.id))

do.check.do15 <-
  bind_rows(
    do.check(x1.sc21.sing.do15)%>%missing.desc.adj(do.adj = 15),
    do.check(x1.sc19.sing.do15)%>%missing.desc.adj(do.adj = 15),
    do.check(x1.sc17.sing.do15)%>%missing.desc.adj(do.adj = 15),
    do.check(x1.sc2.sing.do15)%>%missing.desc.adj(do.adj = 15),
    do.check(x1.sc4.sing.do15)%>%missing.desc.adj(do.adj = 15),
    do.check(x1.sc6.sing.do15)%>%missing.desc.adj(do.adj = 15),
    do.check(x1.sc23.sing.do15)%>%missing.desc.adj(do.adj = 15),
    do.check(x1.sc25.sing.do15)%>%missing.desc.adj(do.adj = 15),
    do.check(x1.sc26.sing.do15)%>%missing.desc.adj(do.adj = 15)
  )%>%
  dplyr::mutate(method = "wald", do = 0.15, hyp = "H0")

saveRDS(do.check.do15, "cluster/out/overall/do.check.wald.15.rds")

h0.sing.sum(x1.sc17.sing.do15)%>%missing.desc.adj(do.adj = 15)%>%filter(strategy=="cca")

h0.sing.do15 <-
  bind_rows(
    h0.sing.sum(x1.sc21.sing.do15)%>%missing.desc.adj(do.adj = 15),
    h0.sing.sum(x1.sc19.sing.do15)%>%missing.desc.adj(do.adj = 15),
    h0.sing.sum(x1.sc17.sing.do15)%>%missing.desc.adj(do.adj = 15),
    h0.sing.sum(x1.sc2.sing.do15)%>%missing.desc.adj(do.adj = 15),
    h0.sing.sum(x1.sc4.sing.do15)%>%missing.desc.adj(do.adj = 15),
    h0.sing.sum(x1.sc6.sing.do15)%>%missing.desc.adj(do.adj = 15),
    h0.sing.sum(x1.sc23.sing.do15)%>%missing.desc.adj(do.adj = 15),
    h0.sing.sum(x1.sc25.sing.do15)%>%missing.desc.adj(do.adj = 15),
    h0.sing.sum(x1.sc26.sing.do15)%>%missing.desc.adj(do.adj = 15)
  )%>%
  dplyr::mutate(method = "wald")

saveRDS(h0.sing.do15, "cluster/out/overall/h0.sing.wald.15.rds")


### MICE ###

x1.sc21.mice.do15 <- readRDS("cluster/out/wn/2xcont/do15/cont2xH0_wn_mice_sc21_do15_param1.rds")
x1.sc19.mice.do15 <- readRDS("cluster/out/wn/2xcont/do15/cont2xH0_wn_mice_sc19_do15_param1.rds")
x1.sc17.mice.do15 <- readRDS("cluster/out/wn/2xcont/do15/cont2xH0_wn_mice_sc17_do15_param1.rds")
x1.sc2.mice.do15 <- readRDS("cluster/out/wn/2xcont/do15/cont2xH0_wn_mice_sc2_do15_param1.rds")
x1.sc4.mice.do15 <- readRDS("cluster/out/wn/2xcont/do15/cont2xH0_wn_mice_sc4_do15_param1.rds")
x1.sc6.mice.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_mice_sc6_do15_param1.rds")
x1.sc23.mice.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_mice_sc23_do15_param1.rds")
x1.sc26.mice.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_mice_sc26_do15_param11.rds")


h0.mice.sum.wn(x1.sc21.mice.do15)
h0.mice.sum.wn(x1.sc19.mice.do15)
h0.mice.sum.wn(x1.sc17.mice.do15)
h0.mice.sum.wn(x1.sc2.mice.do15)
h0.mice.sum.wn(x1.sc4.mice.do15)
h0.mice.sum(x1.sc6.mice.do15)
h0.mice.sum(x1.sc23.mice.do15)
h0.mice.sum(x1.sc26.mice.do15)



