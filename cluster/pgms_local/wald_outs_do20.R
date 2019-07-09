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
ll <- seq(1,30,1)

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

ll <- seq(1,30,1)

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
