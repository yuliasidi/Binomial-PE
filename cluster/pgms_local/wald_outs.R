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
source("funs/pcheck.cca.R") 




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
x1.sc18.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc18_do20_param1.rds")
x1.sc20.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc20_do20_param1.rds")
x1.sc22.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc22_do20_param1.rds")
x1.sc24.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc24_do20_param1.rds")
x1.sc27.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc27_do20_param1.rds")
x1.sc28.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc28_do20_param1.rds")
x1.sc29.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc29_do20_param1.rds")
x1.sc30.sing <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc30_do20_param1.rds")





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
    full.check(x1.sc22.sing, 22),
    full.check(x1.sc23.sing, 23),
    full.check(x1.sc24.sing, 24),
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

do.check.do20 <-
  bind_rows(
    do.check(x1.sc1.sing),
    do.check(x1.sc2.sing),
    do.check(x1.sc3.sing),
    do.check(x1.sc4.sing),
    do.check(x1.sc5.sing),
    do.check(x1.sc6.sing),
    do.check(x1.sc7.sing),
    do.check(x1.sc8.sing),
    do.check(x1.sc9.sing),
    do.check(x1.sc10.sing),
    do.check(x1.sc11.sing),
    do.check(x1.sc12.sing),
    do.check(x1.sc13.sing),
    do.check(x1.sc14.sing),
    do.check(x1.sc15.sing),
    do.check(x1.sc16.sing),
    do.check(x1.sc17.sing),
    do.check(x1.sc18.sing),
    do.check(x1.sc19.sing),
    do.check(x1.sc20.sing),
    do.check(x1.sc21.sing),
    do.check(x1.sc22.sing),
    do.check(x1.sc23.sing),
    do.check(x1.sc24.sing),
    do.check(x1.sc25.sing),
    do.check(x1.sc26.sing),
    do.check(x1.sc27.sing),
    do.check(x1.sc28.sing),
    do.check(x1.sc29.sing),
    do.check(x1.sc30.sing))%>%
    dplyr::mutate(method = "wald", do = 0.2, hyp = "H0")

saveRDS(do.check.do20, "cluster/out/overall/do.check.wald.20.rds")

do.check(x1.sc21.sing.h1)
do.check(x1.sc2.sing.h1)

h0.sing <-
  bind_rows(
    h0.sing.sum(x1.sc1.sing),
    h0.sing.sum(x1.sc2.sing),
    h0.sing.sum(x1.sc3.sing),
    h0.sing.sum(x1.sc4.sing),
    h0.sing.sum(x1.sc5.sing),
    h0.sing.sum(x1.sc6.sing),
    h0.sing.sum(x1.sc7.sing),
    h0.sing.sum(x1.sc8.sing),
    h0.sing.sum(x1.sc9.sing),
    h0.sing.sum(x1.sc10.sing),
    h0.sing.sum(x1.sc11.sing),
    h0.sing.sum(x1.sc12.sing),
    h0.sing.sum(x1.sc13.sing),
    h0.sing.sum(x1.sc14.sing),
    h0.sing.sum(x1.sc15.sing),
    h0.sing.sum(x1.sc16.sing),
    h0.sing.sum(x1.sc17.sing),
    h0.sing.sum(x1.sc18.sing),
    h0.sing.sum(x1.sc19.sing),
    h0.sing.sum(x1.sc20.sing),
    h0.sing.sum(x1.sc21.sing),
    h0.sing.sum(x1.sc22.sing),
    h0.sing.sum(x1.sc23.sing),
    h0.sing.sum(x1.sc24.sing),
    h0.sing.sum(x1.sc25.sing),
    h0.sing.sum(x1.sc26.sing),
    h0.sing.sum(x1.sc27.sing),
    h0.sing.sum(x1.sc28.sing),
    h0.sing.sum(x1.sc29.sing),
    h0.sing.sum(x1.sc30.sing)
  )%>%
  dplyr::mutate(method = "wald")


h0.sing.pcheck.mcar<-
  pcheck.cca(h0.sing,  miss.type = "mcar")

h0.sing.pcheck.mar<-
  pcheck.cca(h0.sing,  miss.type = "mar")

h0.sing.pcheck.mnar<-
  pcheck.cca(h0.sing,  miss.type = "mnar")

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

ll <- c(1,3,5,7,8,22,24,27,28,29)




x1.sc9.mice <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc9_do20_param1.rds")
h0.mice.sum(x1.sc9.mice)

x1.sc29.mice <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc29_do20_param1.rds")
h0.mice.sum(x1.sc29.mice)


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
x1.sc25.mice.do15 <- readRDS("cluster/out/wald/2xcont/do15/cont2xH0_wald_mice_sc25_do15_param1.rds")

h0.mice.do15<-
  bind_rows(
    h0.mice.sum(x1.sc21.mice.do15),
    h0.mice.sum(x1.sc19.mice.do15),
    h0.mice.sum(x1.sc17.mice.do15),
    h0.mice.sum(x1.sc2.mice.do15),
    h0.mice.sum(x1.sc4.mice.do15),
    h0.mice.sum(x1.sc6.mice.do15),
    h0.mice.sum(x1.sc23.mice.do15),
    h0.mice.sum(x1.sc26.mice.do15),
    h0.mice.sum(x1.sc25.mice.do15)
)%>%
  dplyr::mutate(method = "wald", N = num.n.mi, M = num.m.mi)

saveRDS(h0.mice.do15, "cluster/out/overall/h0.mice.wald.15.rds")

##################
#### do = 10% ####
##################

x1.sc21.sing.do10 <- readRDS("cluster/out/wald/2xcont/do10/cont2xH0_wald_sing_sc21_do10_param1.rds")
x1.sc19.sing.do10 <- readRDS("cluster/out/wald/2xcont/do10/cont2xH0_wald_sing_sc19_do10_param1.rds")
x1.sc17.sing.do10 <- readRDS("cluster/out/wald/2xcont/do10/cont2xH0_wald_sing_sc17_do10_param1.rds")
x1.sc2.sing.do10 <- readRDS("cluster/out/wald/2xcont/do10/cont2xH0_wald_sing_sc2_do10_param1.rds")
x1.sc4.sing.do10 <- readRDS("cluster/out/wald/2xcont/do10/cont2xH0_wald_sing_sc4_do10_param1.rds")
x1.sc6.sing.do10 <- readRDS("cluster/out/wald/2xcont/do10/cont2xH0_wald_sing_sc6_do10_param1.rds")
x1.sc23.sing.do10 <- readRDS("cluster/out/wald/2xcont/do10/cont2xH0_wald_sing_sc23_do10_param1.rds")
x1.sc25.sing.do10 <- readRDS("cluster/out/wald/2xcont/do10/cont2xH0_wald_sing_sc25_do10_param1.rds")
x1.sc26.sing.do10 <- readRDS("cluster/out/wald/2xcont/do10/cont2xH0_wald_sing_sc26_do10_param1.rds")

full.type1.do10 <- 
  bind_rows(
    full.check(x1.sc21.sing.do10, 21),
    full.check(x1.sc19.sing.do10, 19),
    full.check(x1.sc17.sing.do10, 17),
    full.check(x1.sc2.sing.do10, 2),
    full.check(x1.sc4.sing.do10, 4),
    full.check(x1.sc6.sing.do10, 6),
    full.check(x1.sc23.sing.do10, 23),
    full.check(x1.sc25.sing.do10, 25),
    full.check(x1.sc26.sing.do10, 26)
  )%>%
  dplyr::mutate(method = "wald")

full.type1 <- readRDS("cluster/out/overall/full.type1.wald.rds")

#compare full datasets between simulations for do=20% and do=15%, should be exactly the same
compare::compare(full.type1%>%arrange(scenario.id)%>%
                   filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26)),
                 full.type1.do10%>%arrange(scenario.id))

do.check.do10 <-
  bind_rows(
    do.check(x1.sc21.sing.do10)%>%missing.desc.adj(do.adj = 10),
    do.check(x1.sc19.sing.do10)%>%missing.desc.adj(do.adj = 10),
    do.check(x1.sc17.sing.do10)%>%missing.desc.adj(do.adj = 10),
    do.check(x1.sc2.sing.do10)%>%missing.desc.adj(do.adj = 10),
    do.check(x1.sc4.sing.do10)%>%missing.desc.adj(do.adj = 10),
    do.check(x1.sc6.sing.do10)%>%missing.desc.adj(do.adj = 10),
    do.check(x1.sc23.sing.do10)%>%missing.desc.adj(do.adj = 10),
    do.check(x1.sc25.sing.do10)%>%missing.desc.adj(do.adj = 10),
    do.check(x1.sc26.sing.do10)%>%missing.desc.adj(do.adj = 10)
  )%>%
  dplyr::mutate(method = "wald", do = 0.10, hyp = "H0")

saveRDS(do.check.do10, "cluster/out/overall/do.check.wald.10.rds")


h0.sing.do10 <-
  bind_rows(
    h0.sing.sum(x1.sc21.sing.do10)%>%missing.desc.adj(do.adj = 10),
    h0.sing.sum(x1.sc19.sing.do10)%>%missing.desc.adj(do.adj = 10),
    h0.sing.sum(x1.sc17.sing.do10)%>%missing.desc.adj(do.adj = 10),
    h0.sing.sum(x1.sc2.sing.do10)%>%missing.desc.adj(do.adj = 10),
    h0.sing.sum(x1.sc4.sing.do10)%>%missing.desc.adj(do.adj = 10),
    h0.sing.sum(x1.sc6.sing.do10)%>%missing.desc.adj(do.adj = 10),
    h0.sing.sum(x1.sc23.sing.do10)%>%missing.desc.adj(do.adj = 10),
    h0.sing.sum(x1.sc25.sing.do10)%>%missing.desc.adj(do.adj = 10),
    h0.sing.sum(x1.sc26.sing.do10)%>%missing.desc.adj(do.adj = 10)
  )%>%
  dplyr::mutate(method = "wald")

saveRDS(h0.sing.do10, "cluster/out/overall/h0.sing.wald.10.rds")


### MICE ###

x1.sc6.mice.do10.1 <- readRDS("cluster/out/wald/2xcont/do10/cont2xH0_wald_mice_sc6_do10_param1.rds")
x1.sc6.mice.do10.2 <- readRDS("cluster/out/wald/2xcont/do10/cont2xH0_wald_mice_sc6_do10_param11.rds")
x1.sc6.mice.do10.sum <- append(x1.sc6.mice.do10.1, x1.sc6.mice.do10.2)%>%
  h0.mice.sum()

h0.mice.do10 <-
  map_df(list.files("cluster/out/wald/2xcont/do10/", "cont2xH0_wald_mice", full.names = T), 
    .f = function(file) {
      df <- readRDS(file)
      h0.mice.sum(df)
    })%>%
  dplyr::filter(scenario.id!=6)%>%
  bind_rows(x1.sc6.mice.do10.sum)%>%
  dplyr::mutate(method = "wald", N = num.n.mi, M = num.m.mi)

saveRDS(h0.mice.do10, "cluster/out/overall/h0.mice.wald.10.rds")


##################
#### do = 5% ####
##################


ll <- c(2,4,6,17,19,21,23,25,26)

full.type1.do5 <-
  map_df(ll, 
         .f = function(sc) {
           dt <- readRDS(list.files("cluster/out/wald/2xcont/do5/", paste0("cont2xH0_wald_sing_sc", sc, "_"), full.names = T))
           full.check(dt, sc)
         })%>%
  dplyr::mutate(method = "wald")

full.type1 <- readRDS("cluster/out/overall/full.type1.wald.rds")

#compare full datasets between simulations for do=20% and do=15%, should be exactly the same
compare::compare(full.type1%>%arrange(scenario.id)%>%
                   filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26)),
                 full.type1.do5%>%arrange(scenario.id))

do.check.do5 <-
  map_df(list.files("cluster/out/wald/2xcont/do5/","cont2xH0_wald_sing_sc", full.names = T), 
         .f = function(file) {
           dt <- readRDS(file)
           do.check(dt)
         })%>%
  dplyr::mutate(method = "wald", do = 0.05, hyp = "H0")

saveRDS(do.check.do5, "cluster/out/overall/do.check.wald.5.rds")


h0.sing.do5 <-
  map_df(list.files("cluster/out/wald/2xcont/do5/","cont2xH0_wald_sing_sc", full.names = T), 
         .f = function(file) {
           dt <- readRDS(file)
           h0.sing.sum(dt)
         })%>%
  dplyr::mutate(method = "wald")

h0.sing.do5<-
  h0.sing.do5%>%
  dplyr::mutate(missing.desc = ifelse(is.na(missing.desc)==T, "  0%", missing.desc))

h0.sing.pcheck.mcar<-
  pcheck.cca(h0.sing.do5,  miss.type = "mcar")

h0.sing.pcheck.mar<-
  pcheck.cca(h0.sing.do5,  miss.type = "mar")

h0.sing.pcheck.mnar<-
  pcheck.cca(h0.sing.do5,  miss.type = "mnar")

saveRDS(h0.sing.do5, "cluster/out/overall/h0.sing.wald.5.rds")

### MICE ###

x1.sc25.mice.do5.condor <- readRDS("cluster/out/wald/2xcont/do5/cont2xH0_wald_mice_sc25_do5_param1.rds")
x1.sc25.mice.do5.local <- readRDS("cluster/out/wald/2xcont/do5/cont2xH0_wald_mice_sc25_do5_param11_local.rds")
x1.sc25.mice.do5 <- append(x1.sc25.mice.do5.condor, x1.sc25.mice.do5.local)
remove(x1.sc25.mice.do5.condor, x1.sc25.mice.do5.local)


x1.sc26.mice.do5.condor <- readRDS("cluster/out/wald/2xcont/do5/cont2xH0_wald_mice_sc26_do5_param1.rds")
x1.sc26.mice.do5.local <- readRDS("cluster/out/wald/2xcont/do5/cont2xH0_wald_mice_sc26_do5_param11_local.rds")
x1.sc26.mice.do5 <- append(x1.sc26.mice.do5.condor, x1.sc26.mice.do5.local)
remove(x1.sc26.mice.do5.condor, x1.sc26.mice.do5.local)

ll <- c(2,4,6,17,19,21,23)

h0.mice.do5 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/wald/2xcont/do5/", paste0("cont2xH0_wald_mice_sc", sc, "_"), full.names = T))
           h0.mice.sum(df)
         })%>%
  bind_rows(
    h0.mice.sum(x1.sc25.mice.do5),
    h0.mice.sum(x1.sc26.mice.do5)
    )%>%
  dplyr::mutate(method = "wald", N = num.n.mi, M = num.m.mi)

saveRDS(h0.mice.do5, "cluster/out/overall/h0.mice.wald.5.rds")


##### Different k's ############

x1.sc21.sing1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc21_do20_param1.rds")
x1.sc21.sing2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc21_do20_param11.rds")
x1.sc21.sing <- append(x1.sc21.sing1,x1.sc21.sing2)
remove(x1.sc21.sing1, x1.sc21.sing2)

x1.sc21.mice1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc21_do20_param1.rds")
x1.sc21.mice2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc21_do20_param11.rds")
x1.sc21.mice <- append(x1.sc21.mice1,x1.sc21.mice2)
remove(x1.sc21.mice1, x1.sc21.mice2)

x1.sc21.mice.k1 <- readRDS("cluster/out/wald/2xcont/change_k/cont2xH0_wald_mice_sc21_do20_param1_k1.rds")
x1.sc21.mice.k2 <- readRDS("cluster/out/wald/2xcont/change_k/cont2xH0_wald_mice_sc21_do20_param1_k2.rds")
x1.sc21.mice.k3 <- readRDS("cluster/out/wald/2xcont/change_k/cont2xH0_wald_mice_sc21_do20_param1_k3.rds")
x1.sc21.mice.k4 <- readRDS("cluster/out/wald/2xcont/change_k/cont2xH0_wald_mice_sc21_do20_param1_k4.rds")

diffk.wald.sc21.do20 <-
  bind_rows(
    h0.mice.sum(x1.sc21.mice),
    h0.mice.sum(x1.sc21.mice.k1),
    h0.mice.sum(x1.sc21.mice.k2),
    h0.mice.sum(x1.sc21.mice.k3),
    h0.mice.sum(x1.sc21.mice.k4))%>%
  dplyr::rename(type1.mice = type1,
                bias.mice = mean.bias)%>%
  dplyr::select(-c(missing.desc))%>%
  dplyr::right_join(h0.sing.sum(x1.sc21.sing)%>%
                      dplyr::filter(grepl("mnar", missing)>0, strategy == "cca"),
                    by = c("scenario.id", "missing", "do"))

diffk.wald.sc21.do20 <-
  diffk.wald.sc21.do20%>%
  dplyr::mutate(k.T = gsub('normal','N',k.T.spec),
                k.C = gsub('normal','N',k.C.spec))


library(ggplot2)

diffk.mnar1.wald.sc21.do20 <-
diffk.wald.sc21.do20%>%
dplyr::filter(missing.desc=="p_T_obs > p_T_full")%>%
  ggplot(aes(x=k.T,y=type1.mice)) + 
  geom_point() + 
  geom_hline(yintercept=diffk.wald.sc21.do20$type1[1]) + 
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  scale_y_continuous(breaks = seq(0, 0.22, 0.025), limits = c(0, .23)) +
  labs(y = "Empirical Type-I error",
       x = "k_T Distribution") +
  theme_bw() +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/diffk_mnar1_wald_sc21_do20.pdf")
diffk.mnar1.wald.sc21.do20
dev.off()

diffk.mnar2.wald.sc21.do20 <-
  diffk.wald.sc21.do20%>%
  dplyr::filter(missing.desc!="p_T_obs > p_T_full")%>%
  ggplot(aes(x=k.C,y=type1.mice)) + 
  geom_point() + 
  geom_hline(yintercept=diffk.wald.sc21.do20$type1[10]) + 
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  scale_y_continuous(breaks = seq(0, 0.15, 0.025), limits = c(0, .15)) +
  labs(y = "Empirical Type-I error",
       x = "k_C Distribution") +
  theme_bw() +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/diffk_mnar2_wald_sc21_do20.pdf")
diffk.mnar2.wald.sc21.do20
dev.off()

#########################################
# MICE without nesting for scenario 21 ##
#########################################

x1.sc21.mice.nonest <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc21_do20_nonest.rds")

x1.nonest.ex <-
  x1.sc21.mice.nonest%>%
    purrr::map_df(.f=function(x) x$ci.miss,.id = 'sim')%>%
    unnest()%>%
    unnest()%>%
  mutate(bias = round((qbar-M2)/M2,4))%>%
  dplyr::group_by(scenario.id, strategy, missing)%>%
  dplyr::summarise(type1=mean(reject.h0), mean.bias = mean(bias))%>%
  miss.desc()%>%
  dplyr::mutate(do = 0.2)%>%
  bind_rows(
    h0.sing.sum(x1.sc21.sing)%>%
      filter(grepl("mnar", missing)>0, strategy=="cca")%>%
      dplyr::select(-c(mean_pc, mean_pt))
  )

saveRDS(x1.nonest.ex, "cluster/out/wald/2xcont/nonest/sc21_do20_nonest.rds")
