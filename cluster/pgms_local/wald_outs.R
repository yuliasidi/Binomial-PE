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



################################################
# Empirical type-I error - Fully observed data #
################################################

ll.sing1 <- c(2,3,5,7,17,19,21)
ll.sing2 <- c(1,4,6,seq(8,16,1),18,20,seq(22,30,1))

full.type1 <-
  map_df(ll.sing1, 
         .f = function(sc) {
           df1 <- readRDS(list.files("cluster/out/wald/2xcont/", 
                                    sprintf("cont2xH0_wald_sing_sc%s_do20_param1.rds", sc), 
                                    full.names = T))
           df2 <- readRDS(list.files("cluster/out/wald/2xcont/", 
                                     sprintf("cont2xH0_wald_sing_sc%s_do20_param11.rds", sc), 
                                     full.names = T))
           df <- append(df1,df2)
           full.check(df, sc)
         })%>%
  bind_rows( map_df(ll.sing2, 
                     .f = function(sc) {
                       df <- readRDS(list.files("cluster/out/wald/2xcont/", 
                                                 sprintf("cont2xH0_wald_sing_sc%s_do20_param1.rds", sc), 
                                                 full.names = T))
                       full.check(df, sc)
                     }))%>%
  dplyr::mutate(method = "wald")


#x1.sc21.bw1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_bw_sc21_do20_param1.rds")
#x1.sc21.bw2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_bw_sc21_do20_param11.rds")
#x1.sc21.bw <- append(x1.sc21.bw1,x1.sc21.bw2)
#remove(x1.sc21.bw1, x1.sc21.bw2)

saveRDS(full.type1%>%
          dplyr::arrange(scenario.id), "cluster/out/overall/full.type1.wald.rds")

################################################
# Empirical power - Fully observed data #
################################################


#check power
ll <- c(seq(1,17,1),21)

full.power <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/wald/2xcont/", 
                                     sprintf("cont2xH1_wald_sing_sc%s_do20_param1.rds", sc), 
                                     full.names = T))
           full.check(df, sc)
         })%>%
  dplyr::mutate(method = "wald")

saveRDS(full.power%>%
          dplyr::arrange(scenario.id), "cluster/out/overall/full.power.wald.rds")


#########################################
# Check do rates, data under H0, DO=20% #
########################################

ll.sing1 <- c(2,3,5,7,17,19,21)
ll.sing2 <- c(1,4,6,seq(8,16,1),18,20,seq(22,30,1))

do.check.do20 <-
  map_df(ll.sing1, 
         .f = function(sc) {
           df1 <- readRDS(list.files("cluster/out/wald/2xcont/", 
                                     sprintf("cont2xH0_wald_sing_sc%s_do20_param1.rds", sc), 
                                     full.names = T))
           df2 <- readRDS(list.files("cluster/out/wald/2xcont/", 
                                     sprintf("cont2xH0_wald_sing_sc%s_do20_param11.rds", sc), 
                                     full.names = T))
           df <- append(df1,df2)
           do.check(df)
         })%>%
  bind_rows( map_df(ll.sing2, 
                    .f = function(sc) {
                      df <- readRDS(list.files("cluster/out/wald/2xcont/", 
                                               sprintf("cont2xH0_wald_sing_sc%s_do20_param1.rds", sc), 
                                               full.names = T))
                      do.check(df)
                    }))%>%
  dplyr::mutate(method = "wald", do = 0.2, hyp = "H0")


saveRDS(do.check.do20%>%
          dplyr::arrange(scenario.id), "cluster/out/overall/do.check.wald.20.rds")


#########################################
# Check do rates, data under H1, DO=20% #
#########################################

ll <- c(seq(1,17,1),21)

do.check.do20.h1 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/wald/2xcont/", 
                                    sprintf("cont2xH1_wald_sing_sc%s_do20_param1.rds", sc), 
                                    full.names = T))
           do.check(df)
         })%>%
  dplyr::mutate(method = "wald", do = 0.2, hyp = "H1")

saveRDS(do.check.do20.h1%>%
          dplyr::arrange(scenario.id), "cluster/out/overall/do.check.wald.20.h1.rds")

df <- readRDS(list.files("cluster/out/wald/2xcont/", 
                         sprintf("cont2xH1_wald_sing_sc%s_do20_param1.rds", sc), 
                         full.names = T))
do.check(df)
do.check(x1.sc2.sing.h1)

##########################################################################
# Empirical type-I error - Incomplete, single imputation strategy, DO=20%#
##########################################################################

ll.sing1 <- c(2,3,5,7,17,19,21)
ll.sing2 <- c(1,4,6,seq(8,16,1),18,20,seq(22,30,1))

h0.sing <-
  map_df(ll.sing1, 
         .f = function(sc) {
           df1 <- readRDS(list.files("cluster/out/wald/2xcont/", 
                                     sprintf("cont2xH0_wald_sing_sc%s_do20_param1.rds", sc), 
                                     full.names = T))
           df2 <- readRDS(list.files("cluster/out/wald/2xcont/", 
                                     sprintf("cont2xH0_wald_sing_sc%s_do20_param11.rds", sc), 
                                     full.names = T))
           df <- append(df1,df2)
           h0.sing.sum(df)
         })%>%
  bind_rows( map_df(ll.sing2, 
                    .f = function(sc) {
                      df <- readRDS(list.files("cluster/out/wald/2xcont/", 
                                               sprintf("cont2xH0_wald_sing_sc%s_do20_param1.rds", sc), 
                                               full.names = T))
                      h0.sing.sum(df)
                    }))%>%
  dplyr::mutate(method = "wald")


h0.sing.pcheck.mcar<-
  pcheck.cca(h0.sing,  miss.type = "mcar")

h0.sing.pcheck.mar<-
  pcheck.cca(h0.sing,  miss.type = "mar")

h0.sing.pcheck.mnar<-
  pcheck.cca(h0.sing,  miss.type = "mnar")

saveRDS(h0.sing, "cluster/out/overall/h0.sing.wald.20.rds")

##########################################################################
# Empirical type-I error - Incomplete, MICE imputation strategy, DO=20%  #
##########################################################################

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

ll <- c(1, 3, 5, seq(7, 16, 1), 18, 20, 22, 24, 25, 27, 28, 29, 30)

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
    h0.mice.sum(x1.sc25.mice),
    map_df(ll, 
           .f = function(sc) {
             df <- readRDS(list.files("cluster/out/wald/2xcont/", 
                                      sprintf("cont2xH0_wald_mice_sc%s_do20_param1.rds", sc), 
                                      full.names = T))
             h0.mice.sum(df)
           })
    
  )%>%
  dplyr::mutate(method = "wald", N = num.n.mi, M = num.m.mi)

saveRDS(h0.mice, "cluster/out/overall/h0.mice.wald.20.rds")

#####################################################################
# Empirical power - Incomplete, single imputation strategy, DO=20%  #
#####################################################################
ll <- seq(1,30,1)

power.sing.20 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/wald/2xcont/", 
                                    sprintf("cont2xH1_wald_sing_sc%s_do20_param1.rds", sc), 
                                    full.names = T))
           h0.sing.sum(df)%>%
             dplyr::select(-mean.bias)%>%
             dplyr::rename(power=type1)
         })%>%
  dplyr::mutate(method = "wald")

saveRDS(power.sing.20, "cluster/out/overall/h1.sing.wald.20.rds")


###################################################################
# Empirical power - Incomplete, MICE imputation strategy, DO=20%  #
###################################################################
ll <- seq(1,30,1)

power.mice.20 <-
  map_df(ll, 
       .f = function(sc) {
         df <- readRDS(list.files("cluster/out/wald/2xcont/", 
                                  sprintf("cont2xH1_wald_mice_sc%s_do20_param1.rds", sc), 
                                  full.names = T))
         h0.mice.sum(df)%>%
           dplyr::select(-mean.bias)%>%
           dplyr::rename(power=type1)
       })%>%
  dplyr::mutate(method = "wald", N = num.n.mi, M = num.m.mi)

saveRDS(power.mice.20, "cluster/out/overall/h1.mice.wald.20.rds")

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
