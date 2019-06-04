library(dplyr)
library(tidyr)
library(purrr)

 
ss <- readRDS("cluster/ss.bounds.rds")
ss <- ss%>%
  dplyr::filter(method == "wald")

source("cluster/pgms/init.R")
source("funs/full.check.R")
source("funs/do.check.R")
source("funs/h0.sing.sum.R")
source("funs/h0.mice.sum.R")
source("funs/plot.type1.R")
source("funs/plot.bias.R")
source("funs/plot.power.R")
source("funs/miss.desc.R")
source("funs/missing.desc.adj.R")



#Read and summarise all the single value imputations
x1.sc21.sing1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc21_do20_param1.rds")
x1.sc21.sing2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc21_do20_param11.rds")
x1.sc21.sing <- append(x1.sc21.sing1,x1.sc21.sing2)
remove(x1.sc21.sing1, x1.sc21.sing2)
x1.sc21.sing.h1 <- readRDS("cluster/out/wald/2xcont/cont2xH1_wald_sing_sc21_do20_param1.rds")


x1.sc19.sing1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc19_do20_param1.rds")
x1.sc19.sing2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc19_do20_param11.rds")
x1.sc19.sing <- append(x1.sc19.sing1,x1.sc19.sing2)
remove(x1.sc19.sing1, x1.sc19.sing2)

x1.sc17.sing1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc17_do20_param1.rds")
x1.sc17.sing2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc17_do20_param11.rds")
x1.sc17.sing <- append(x1.sc17.sing1,x1.sc17.sing2)
remove(x1.sc17.sing1, x1.sc17.sing2)

x1.sc2.sing1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc2_do20_param1.rds")
x1.sc2.sing2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc2_do20_param11.rds")
x1.sc2.sing <- append(x1.sc2.sing1,x1.sc2.sing2)
remove(x1.sc2.sing1, x1.sc2.sing2)
x1.sc2.sing.h1 <- readRDS("cluster/out/wald/2xcont/cont2xH1_wald_sing_sc2_do20_param1.rds")

x1.sc23.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc23_do20_param1.rds")
x1.sc25.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc25_do20_param1.rds")
x1.sc26.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc26_do20_param1.rds")

x1.sc4.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc4_do20_param1.rds")
x1.sc6.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc6_do20_param1.rds")


#x1.sc21.bw1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_bw_sc21_do20_param1.rds")
#x1.sc21.bw2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_bw_sc21_do20_param11.rds")
#x1.sc21.bw <- append(x1.sc21.bw1,x1.sc21.bw2)
#remove(x1.sc21.bw1, x1.sc21.bw2)

x1.sc7.sing1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc7_do20_param1.rds")
x1.sc7.sing2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc7_do20_param11.rds")
x1.sc7.sing <- append(x1.sc7.sing1,x1.sc7.sing2)
remove(x1.sc7.sing1, x1.sc7.sing2)
 

x1.sc5.sing1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc5_do20_param1.rds")
x1.sc5.sing2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc5_do20_param11.rds")
x1.sc5.sing <- append(x1.sc5.sing1,x1.sc5.sing2)
remove(x1.sc5.sing1, x1.sc5.sing2)

x1.sc3.sing1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc3_do20_param1.rds")
x1.sc3.sing2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc3_do20_param11.rds")
x1.sc3.sing <- append(x1.sc3.sing1,x1.sc3.sing2)
remove(x1.sc3.sing1, x1.sc3.sing2)

x1.sc1.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc1_do20_param1.rds")
x1.sc8.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc8_do20_param1.rds")
x1.sc9.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc9_do20_param1.rds")
x1.sc10.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc10_do20_param1.rds")
x1.sc11.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc11_do20_param1.rds")
x1.sc12.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc12_do20_param1.rds")
x1.sc13.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc13_do20_param1.rds")
x1.sc14.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc14_do20_param1.rds")
x1.sc15.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc15_do20_param1.rds")
x1.sc16.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc16_do20_param1.rds")

#check p_C, p_T and type1/power for full data
full.type1<-
  bind_rows(
    full.check(x1.sc1.sing, 1),
    full.check(x1.sc2.sing, 2),
    full.check(x1.sc3.sing, 3),
    full.check(x1.sc4.sing, 4),
    full.check(x1.sc5.sing, 5),
    full.check(x1.sc6.sing, 6),
    full.check(x1.sc7.sing, 7),
    full.check(x1.sc8.sing, 8),
    full.check(x1.sc9.sing, 9),
    full.check(x1.sc10.sing, 10),
    full.check(x1.sc11.sing, 11),
    full.check(x1.sc12.sing, 12),
    full.check(x1.sc13.sing, 13),
    full.check(x1.sc14.sing, 14),
    full.check(x1.sc15.sing, 15),
    full.check(x1.sc16.sing, 16),
    full.check(x1.sc17.sing, 17),
    full.check(x1.sc18.sing, 18),
    full.check(x1.sc19.sing, 19),
    full.check(x1.sc20.sing, 20),
    full.check(x1.sc21.sing, 21),
    full.check(x1.sc21.sing, 22),
    full.check(x1.sc23.sing, 23),
    full.check(x1.sc23.sing, 24),
    full.check(x1.sc25.sing, 25),
    full.check(x1.sc26.sing, 26),
    full.check(x1.sc27.sing, 27),
    full.check(x1.sc28.sing, 28),
    full.check(x1.sc29.sing, 29),
    full.check(x1.sc30.sing, 30)
  )%>%
  dplyr::mutate(method = "wald")

saveRDS(full.type1, "cluster/out/overall/full.type1.wald.rds")


#check power

full.check(x1.sc21.sing.h1, 21)
full.check(x1.sc2.sing.h1, 2)


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
  dplyr::mutate(method = "wald", do = 0.2, hyp = "H0")

saveRDS(do.check, "cluster/out/overall/do.check.wald.20.rds")

do.check(x1.sc21.sing.h1)
do.check(x1.sc2.sing.h1)

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
  dplyr::mutate(method = "wald")

saveRDS(h0.sing, "cluster/out/overall/h0.sing.wald.20.rds")


h0.sing.sum(x1.sc21.sing.h1)%>%filter(strategy=="cca")
h0.sing.sum(x1.sc2.sing.h1)%>%filter(strategy=="cca")

#Read and summarise all the MICE imputations

x1.sc21.mice1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc21_do20_param1.rds")
x1.sc21.mice2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc21_do20_param11.rds")
x1.sc21.mice <- append(x1.sc21.mice1,x1.sc21.mice2)
remove(x1.sc21.mice1, x1.sc21.mice2)
x1.sc21.mice.h1 <- readRDS("cluster/out/wald/2xcont/cont2xH1_wald_mice_sc21_do20_param1.rds")


x1.sc19.mice1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc19_do20_param1.rds")
x1.sc19.mice2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc19_do20_param11.rds")
x1.sc19.mice <- append(x1.sc19.mice1,x1.sc19.mice2)
remove(x1.sc19.mice1, x1.sc19.mice2)

x1.sc17.mice.mnar11 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc17_do20_param1_mnar1.rds")
x1.sc17.mice.mnar12 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc17_do20_param11_mnar1.rds")
x1.sc17.mice.mnar21 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc17_do20_param1_mnar2.rds")
x1.sc17.mice.mnar22 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc17_do20_param11_mnar2.rds")
x1.sc17.mice <- append(append(x1.sc17.mice.mnar11, x1.sc17.mice.mnar12),
                             append(x1.sc17.mice.mnar21, x1.sc17.mice.mnar22))
remove(x1.sc17.mice.mnar11, x1.sc17.mice.mnar12, x1.sc17.mice.mnar21, x1.sc17.mice.mnar22)


x1.sc2.mice1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc2_do20_param1.rds")
x1.sc2.mice2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc2_do20_param11.rds")
x1.sc2.mice3 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc2_do20_param11_mnar2.rds")
x1.sc2.mice <- append(x1.sc2.mice1, append(x1.sc2.mice2, x1.sc2.mice3))
remove(x1.sc2.mice1, x1.sc2.mice2, x1.sc2.mice3)

x1.sc4.mice1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc4_do20_param1.rds")
x1.sc4.mice2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc4_do20_param11.rds")
x1.sc4.mice1.mnar2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc4_do20_param1_mnar2.rds")
x1.sc4.mice2.mnar2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc4_do20_param11_mnar2.rds")
x1.sc4.mice <- append(append(x1.sc4.mice1,x1.sc4.mice2),append(x1.sc4.mice1.mnar2,x1.sc4.mice2.mnar2))
remove(x1.sc4.mice1, x1.sc4.mice2, x1.sc4.mice1.mnar2, x1.sc4.mice2.mnar2)


x1.sc26.mice1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc26_do20_param1.rds")
x1.sc26.mice2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc26_do20_param11.rds")
x1.sc26.mice1.mnar2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc26_do20_param1_mnar2.rds")
x1.sc26.mice2.mnar2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc26_do20_param11_mnar2.rds")
x1.sc26.mice <- append(append(x1.sc26.mice1,x1.sc26.mice2),append(x1.sc26.mice1.mnar2,x1.sc26.mice2.mnar2))
remove(x1.sc26.mice1, x1.sc26.mice2, x1.sc26.mice1.mnar2, x1.sc26.mice2.mnar2)


x1.sc6.mice1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc6_do20_param1.rds")
x1.sc6.mice2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc6_do20_param11.rds")
x1.sc6.mice_mnar21 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc6_do20_param1_mnar2.rds")
x1.sc6.mice <- append(x1.sc6.mice1, append(x1.sc6.mice2, x1.sc6.mice_mnar21 ))


x1.sc23.mice1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc23_do20_param1.rds")
x1.sc23.mice2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc23_do20_param11.rds")
x1.sc23.mice1.mnar2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc23_do20_param1_mnar2.rds")
x1.sc23.mice11.mnar2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc23_do20_param11_mnar2.rds")
x1.sc23.mice <- append(append(x1.sc23.mice1,x1.sc23.mice2),append(x1.sc23.mice1.mnar2,x1.sc23.mice11.mnar2))

x1.sc25.mice <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc25_do20_param1.rds")

h0.mice <-
  bind_rows(
    h0.mice.sum(x1.sc21.mice),
    h0.mice.sum(x1.sc19.mice),
    h0.mice.sum(x1.sc17.mice),
    h0.mice.sum(x1.sc2.mice)%>%
      filter(k.C.spec!="normal(1.65, 0.05)"),
    h0.mice.sum(x1.sc4.mice)%>%
      filter(k.C.spec!="normal(1.4, 0.05)"),
    h0.mice.sum(x1.sc26.mice)%>%
      filter(k.C.spec!="normal(1.45, 0.05)",k.C.spec!="normal(1.5, 0.05)"),
    h0.mice.sum(x1.sc23.mice)%>%
      filter(k.C.spec!="normal(1.65, 0.05)",k.C.spec!="normal(1.75, 0.05)"),
    h0.mice.sum(x1.sc6.mice)%>%
      filter(k.C.spec!="normal(1.3, 0.05)"),
    h0.mice.sum(x1.sc25.mice)
    
  )%>%
  dplyr::mutate(method = "wald", N = num.n.mi, M = num.m.mi)

saveRDS(h0.mice, "cluster/out/overall/h0.mice.wald.20.rds")


h0.mice.sum(x1.sc21.mice.h1)

h0.sing.sum(x1.sc2.sing)%>%
  dplyr::filter(strategy=="cca")
h0.sing.sum(x1.sc21.bw)


h0.mice.sum(x1.sc6.mice)



##################
#### do = 15% ####
##################

x1.sc21.sing.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_sing_sc21_do15_param1.rds")
x1.sc19.sing.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_sing_sc19_do15_param1.rds")
x1.sc17.sing.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_sing_sc17_do15_param1.rds")
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

x1.sc21.mice.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_mice_sc21_do15_param1.rds")
x1.sc19.mice.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_mice_sc19_do15_param1.rds")
x1.sc17.mice.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_mice_sc17_do15_param1.rds")
x1.sc2.mice.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_mice_sc2_do15_param1.rds")
x1.sc4.mice.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_mice_sc4_do15_param1.rds")
x1.sc6.mice.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_mice_sc6_do15_param1.rds")
x1.sc23.mice.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_mice_sc23_do15_param1.rds")
x1.sc26.mice.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_mice_sc26_do15_param11.rds")


h0.mice.sum(x1.sc21.mice.do15)
h0.mice.sum(x1.sc19.mice.do15)
h0.mice.sum(x1.sc17.mice.do15)
h0.mice.sum(x1.sc2.mice.do15)
h0.mice.sum(x1.sc4.mice.do15)
h0.mice.sum(x1.sc6.mice.do15)
h0.mice.sum(x1.sc23.mice.do15)
h0.mice.sum(x1.sc26.mice.do15)



##### Different k's ############

x1.sc21.mice1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc21_do20_param1.rds")
x1.sc21.mice2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc21_do20_param11.rds")
x1.sc21.mice <- append(x1.sc21.mice1,x1.sc21.mice2)
remove(x1.sc21.mice1, x1.sc21.mice2)

x1.sc21.mice.k1 <- readRDS("cluster/out/wald/2xcont/change_k/cont2xH0_wald_mice_sc21_do20_param1_k1.rds")
x1.sc21.mice.k2 <- readRDS("cluster/out/wald/2xcont/change_k/cont2xH0_wald_mice_sc21_do20_param1_k2.rds")
x1.sc21.mice.k3 <- readRDS("cluster/out/wald/2xcont/change_k/cont2xH0_wald_mice_sc21_do20_param1_k3.rds")
x1.sc21.mice.k4 <- readRDS("cluster/out/wald/2xcont/change_k/cont2xH0_wald_mice_sc21_do20_param1_k4.rds")

h0.sing.sum(x1.sc21.sing)
h0.mice.sum(x1.sc21.mice)
h0.mice.sum(x1.sc21.mice.k1)
h0.mice.sum(x1.sc21.mice.k2)
h0.mice.sum(x1.sc21.mice.k3)
h0.mice.sum(x1.sc21.mice.k4)



################## Old code ############################
ss.bounds <- readRDS("cluster/ss.bounds.rds")

ss.bounds <- ss.bounds%>%
  dplyr::filter(method=="wald")

ff.n <- expand.grid(scenario = seq(1,30,2), do = c(10,20), setn = 1)

ff.n1 <- ff.n%>%filter(scenario%in%c(11,13,15,17,19,21))



t1.H0 <- pmap_df(as.list(ff.n1), .f = function(scenario, do, setn){
  t <-readRDS(sprintf("cluster/out/wald/outH0_p30_wald%d_%d_set%d.rds", scenario, do, setn))
  t1 <- t%>%
    dplyr::bind_rows()%>%
    dplyr::mutate(setn = setn)
  return(t1)
  
})


t1.H1 <- pmap_df(as.list(ff.n1), .f = function(scenario, do, setn){
  t <-readRDS(sprintf("cluster/out/wald/outH1_p30_wald%d_%d_set%d.rds", scenario, do, setn))
  t1 <- t%>%
    dplyr::bind_rows()%>%
    dplyr::mutate(setn = setn)
  return(t1)
  
})


t2.H0 <- t1.H0%>%
  mutate(bias = round((phat.d-M2)/M2,4))%>%
  dplyr::group_by(scenario.id, strategy, missing, do)%>%
  dplyr::summarise(type1=mean(reject.h0), mean.bias = mean(bias))%>%
  dplyr::mutate(missing.new = case_when(missing=="mar1" ~ "0%",
                                        missing=="mar2" ~ "-5%",
                                        missing=="mar3" & do==0.1 ~ "-10%",
                                        missing=="mar3" & do==0.2 ~ "-15%",
                                        missing=="mar4" & do==0.1 ~  "5%",
                                        missing=="mar4" & do==0.2 ~ "-25%",
                                        missing=="mar5" & do==0.1 ~  "10%",
                                        missing=="mar5" & do==0.2 ~  "5%",
                                        missing=="mar6" ~ "15%",
                                        missing=="mar7" ~ "25%"))%>%
  dplyr::left_join(ss.bounds%>%
                     dplyr::select(scenario.id, p_C, M2, n.arm), by = "scenario.id")

t2.H0$missing.new <- factor(t2.H0$missing.new,levels=unique(t2.H0$missing.new)[c(6,4,3,2,1,5,8,7,9,10)])
t2.H0$flabel <- sprintf('p[C]: %s, Delta: %s, n:%s',t2.H0$p_C,t2.H0$M2,t2.H0$n.arm)

type1.plot.20.mar.ccamice <-
  t2.H0%>%
  filter(strategy%in%c("cca", "mice m=20"), missing!="mcar")%>%
  plot.type1(do.val = 0.2, 
             p.title = "Type-I error: MAR for overall drop-out rate of 20%")

type1.plot.20.mar.bw <-
  t2.H0%>%
  filter(strategy%in%c("best", "worst"), missing!="mcar")%>%
  plot.type1(do.val = 0.2, 
             p.title = "Type-I error: MAR for overall drop-out rate of 20%")

# mcar.ccamice <-
#   t2.H0%>%
#   filter(strategy%in%c("cca", "mice m=20"), missing=="mcar")

type1.plot.10.mar.ccamice <-
  t2.H0%>%
  filter(strategy%in%c("cca", "mice m=20"), missing!="mcar")%>%
  plot.type1(do.val = 0.1, 
             p.title = "Type-I error: MAR for overall drop-out rate of 10%")

type1.plot.10.mar.bw <-
  t2.H0%>%
  filter(strategy%in%c("best", "worst"), missing!="mcar")%>%
  plot.type1(do.val = 0.1, 
             p.title = "Type-I error: MAR for overall drop-out rate of 10%")

bias.plot.10.mar.ccamice <-
  t2.H0%>%
  filter(strategy%in%c("cca", "mice m=20"), missing!="mcar")%>%
  plot.bias(do.val = 0.1, 
            p.title = "Bias: MAR for overall drop-out rate of 10%")

bias.plot.10.mar.bw <-
  t2.H0%>%
  filter(strategy%in%c("best", "worst"), missing!="mcar")%>%
  plot.bias(do.val = 0.1, 
            p.title = "Bias: MAR for overall drop-out rate of 10%")

bias.plot.20.mar.ccamice <-
  t2.H0%>%
  filter(strategy%in%c("cca", "mice m=20"), missing!="mcar")%>%
  plot.bias(do.val = 0.2, 
            p.title = "Bias: MAR for overall drop-out rate of 20%")

bias.plot.20.mar.bw <-
  t2.H0%>%
  filter(strategy%in%c("best", "worst"), missing!="mcar")%>%
  plot.bias(do.val = 0.2, 
            p.title = "Bias: MAR for overall drop-out rate of 20%")

t2.H1 <- t1.H1%>%
  dplyr::group_by(scenario.id, strategy, missing, do)%>%
  dplyr::summarise(power=mean(reject.h0))%>%
  dplyr::mutate(missing.new = case_when(missing=="mar1" ~ "0%",
                                        missing=="mar2" ~ "-5%",
                                        missing=="mar3" & do==0.1 ~ "-10%",
                                        missing=="mar3" & do==0.2 ~ "-15%",
                                        missing=="mar4" & do==0.1 ~  "5%",
                                        missing=="mar4" & do==0.2 ~ "-25%",
                                        missing=="mar5" & do==0.1 ~  "10%",
                                        missing=="mar5" & do==0.2 ~  "5%",
                                        missing=="mar6" ~ "15%",
                                        missing=="mar7" ~ "25%"))%>%
  dplyr::left_join(ss.bounds%>%
                     dplyr::select(scenario.id, p_C, M2, n.arm), by = "scenario.id")

t2.H1$missing.new <- factor(t2.H1$missing.new,levels=unique(t2.H1$missing.new)[c(6,4,3,2,1,5,8,7,9,10)])
t2.H1$flabel <- sprintf('p[C]: %s, Delta: %s, n:%s',t2.H1$p_C,t2.H1$M2,t2.H1$n.arm)

power.plot.20.mar.ccamice <-
  t2.H1%>%
  filter(strategy%in%c("cca", "mice m=20"), missing!="mcar")%>%
  plot.power(do.val = 0.2, 
             p.title = "Power: MAR for overall drop-out rate of 20%")

power.plot.20.mar.bw <-
  t2.H1%>%
  filter(strategy%in%c("best", "worst"), missing!="mcar")%>%
  plot.power(do.val = 0.2, 
             p.title = "Power: MAR for overall drop-out rate of 20%")


power.plot.10.mar.ccamice <-
  t2.H1%>%
  filter(strategy%in%c("cca", "mice m=20"), missing!="mcar")%>%
  plot.power(do.val = 0.1, 
             p.title = "Power: MAR for overall drop-out rate of 10%")

power.plot.10.mar.bw <-
  t2.H1%>%
  filter(strategy%in%c("best", "worst"), missing!="mcar")%>%
  plot.power(do.val = 0.1, 
             p.title = "Power: MAR for overall drop-out rate of 10%")



###########
# Save

pdf("cluster/out/wald/plots/type1_mar10_wald_ccamice.pdf")
type1.plot.10.mar.ccamice
dev.off()

pdf("cluster/out/wald/plots/type1_mar10_wald_bw.pdf")
type1.plot.10.mar.bw
dev.off()

pdf("cluster/out/wald/plots/type1_mar20_wald_ccamice.pdf")
type1.plot.20.mar.ccamice
dev.off()

pdf("cluster/out/wald/plots/type1_mar20_wald_bw.pdf")
type1.plot.20.mar.bw
dev.off()

##### Bias

pdf("cluster/out/wald/plots/bias_mar10_wald_ccamice.pdf")
bias.plot.10.mar.ccamice
dev.off()

pdf("cluster/out/wald/plots/bias_mar10_wald_bw.pdf")
bias.plot.10.mar.bw
dev.off()

pdf("cluster/out/wald/plots/bias_mar20_wald_ccamice.pdf")
bias.plot.20.mar.ccamice
dev.off()

pdf("cluster/out/wald/plots/bias_mar20_wald_bw.pdf")
bias.plot.20.mar.bw
dev.off()


##### Power

pdf("cluster/out/wald/plots/power_mar10_wald_ccamice.pdf")
power.plot.10.mar.ccamice
dev.off()

pdf("cluster/out/wald/plots/power_mar10_wald_bw.pdf")
power.plot.10.mar.bw
dev.off()

pdf("cluster/out/wald/plots/power_mar20_wald_ccamice.pdf")
power.plot.20.mar.ccamice
dev.off()

pdf("cluster/out/wald/plots/power_mar20_wald_bw.pdf")
power.plot.20.mar.bw
dev.off()

######################
t2.H0%>%
  filter(type1<=1.1*0.025, type1>0.9*0.025, missing!="mcar")%>%
  group_by(strategy, do)%>%
  summarise(n())


ff.n <- expand.grid(scenario = seq(1,30,2), do = c(20), setn = 1, rvv = c("p","m"))

ff.n1 <- ff.n%>%filter(scenario%in%c(21))

k<-readRDS("cluster/out/wald/outH0_p30_wald21_20_set1.rds")
k1<-readRDS("cluster/out/wald/outH0_m30_wald21_20_set1.rds")

t1.H0 <- bind_rows(bind_rows(k)%>%
                     mutate(rho = 0.3), 
                   bind_rows(k1)%>%
                     mutate(rho = -0.3))


t2.H0 <- t1.H0%>%
  mutate(bias = round((phat.d-M2)/M2,4))%>%
  dplyr::group_by(scenario.id, strategy, missing, do, rho)%>%
  dplyr::summarise(type1=mean(reject.h0), mean.bias = mean(bias))%>%
  dplyr::mutate(missing.new = case_when(missing=="mar1" ~ "0%",
                                        missing=="mar2" ~ "-5%",
                                        missing=="mar3" & do==0.1 ~ "-10%",
                                        missing=="mar3" & do==0.2 ~ "-15%",
                                        missing=="mar4" & do==0.1 ~  "5%",
                                        missing=="mar4" & do==0.2 ~ "-25%",
                                        missing=="mar5" & do==0.1 ~  "10%",
                                        missing=="mar5" & do==0.2 ~  "5%",
                                        missing=="mar6" ~ "15%",
                                        missing=="mar7" ~ "25%"))%>%
  dplyr::left_join(ss.bounds%>%
                     dplyr::select(scenario.id, p_C, M2, n.arm), by = "scenario.id")

t2.H0$missing.new <- factor(t2.H0$missing.new,levels=unique(t2.H0$missing.new)[c(6,4,3,2,1,5,8,7,9,10)])
t2.H0$flabel <- sprintf('p[C]: %s, Delta: %s, n:%s',t2.H0$p_C,t2.H0$M2,t2.H0$n.arm)

  t2.H0%>%
  filter(strategy%in%c("cca", "mice m=20"), missing!="mcar")%>%
    ggplot(aes(y=missing.new,x=type1,colour=strategy)) + 
    geom_point() + 
    facet_wrap(~rho,ncol=2) + 
    geom_vline(xintercept=c(0.9,1.1)*0.025,
               linetype=2) + 
    labs(x = "Empirical Type-I error",
         y = "Drop-out difference (%C-%T)" ,
         title = "p.title") +
    theme_bw() +
    theme(legend.position = 'bottom')
  