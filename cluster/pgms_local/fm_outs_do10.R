library(dplyr)
library(tidyr)
library(purrr)

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


ss <- readRDS("cluster/ss.bounds.rds")
ss <- ss%>%
  dplyr::filter(method == "fm")

#######################################################
# Check full data under H0, DO=10% vs previous result #
#######################################################

ll <- seq(1,30,1)

full.type1.do10 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/fm/2xcont/do10", 
                                    sprintf("cont2xH0_fm_sing_sc%s_do10_param1.rds", sc), 
                                    full.names = T))
           full.check(df, sc)
         })%>%
  dplyr::mutate(method = "fm")

full.type1 <- readRDS("cluster/out/overall/full.type1.fm.rds")

#compare full datasets between simulations for do=20% and do=10%, should be exactly the same
compare::compare(full.type1%>%arrange(scenario.id),
                 full.type1.do10%>%arrange(scenario.id))

#########################################
# Check do rates, data under H0, DO=10% #
########################################

#check do rates
do.check.do10 <-
  map_df(list.files("cluster/out/fm/2xcont/do10/","cont2xH0_fm_sing_sc", full.names = T), 
         .f = function(file) {
           dt <- readRDS(file)
           do.check(dt)%>%missing.desc.adj(do.adj = 10)
         })%>%
  dplyr::mutate(method = "fm", do = 0.10, hyp = "H0")

saveRDS(do.check.do10, "cluster/out/overall/do.check.fm.10.rds")

#########################################
# Check do rates, data under H1, DO=15% #
#########################################

ll <- seq(1,30,1)

do.check.do15.h1 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/fm/2xcont/", 
                                    sprintf("cont2xH1_fm_sing_sc%s_do15_param1.rds", sc), 
                                    full.names = T))
           do.check(df)%>%missing.desc.adj(do.adj = 15)
         })%>%
  dplyr::mutate(method = "fm", do = 0.15, hyp = "H1")

saveRDS(do.check.do15.h1%>%
          dplyr::arrange(scenario.id), "cluster/out/overall/do.check.fm.15.h1.rds")

##########################################################################
# Empirical type-I error - Incomplete, single imputation strategy, DO=10%#
##########################################################################

h0.sing.do10 <-
  map_df(list.files("cluster/out/fm/2xcont/do10","cont2xH0_fm_sing_sc", full.names = T), 
         .f = function(file) {
           dt <- readRDS(file)
           h0.sing.sum(dt)%>%missing.desc.adj(do.adj = 10)
         })%>%
  dplyr::mutate(method = "fm")

h0.sing.pcheck.mcar<-
  pcheck.cca(h0.sing.do10,  miss.type = "mcar")

h0.sing.pcheck.mar<-
  pcheck.cca(h0.sing.do10,  miss.type = "mar")

h0.sing.pcheck.mnar<-
  pcheck.cca(h0.sing.do10,  miss.type = "mnar")

saveRDS(h0.sing.do10, "cluster/out/overall/h0.sing.fm.10.rds")


##########################################################################
# Empirical type-I error - Incomplete, MICE imputation strategy, DO=10%  #
##########################################################################

ll <- seq(1,30,1)

h0.mice.10 <- map_df(ll, 
                   .f = function(sc) {
                     df <- readRDS(list.files("cluster/out/fm/2xcont/do10", 
                                              sprintf("cont2xH0_fm_mice_sc%s_do10_param1.rds", sc), 
                                              full.names = T))
                     h0.mice.sum(df)%>%missing.desc.adj(do.adj = 10)
                   })%>%
  dplyr::mutate(method = "fm", N = num.n.mi, M = num.m.mi)

saveRDS(h0.mice.10, "cluster/out/overall/h0.mice.fm.10.rds")





##################
#### do = 15% ####
##################


ll <- c(2,4,6,17,19,21,23,25,26)

full.type1.do15 <-
  map_df(ll, 
         .f = function(sc) {
           dt <- readRDS(list.files("cluster/out/fm/2xcont/do15/", paste0("cont2xH0_fm_sing_sc", sc, "_"), full.names = T))
           full.check(dt, sc)
         })%>%
  dplyr::mutate(method = "fm")

full.type1 <- readRDS("cluster/out/overall/full.type1.fm.rds")

#compare full datasets between simulations for do=20% and do=15%, should be exactly the same
compare::compare(full.type1%>%arrange(scenario.id)%>%
                   filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26)),
                 full.type1.do15%>%arrange(scenario.id))

do.check.do15 <-
  map_df(list.files("cluster/out/fm/2xcont/do15/","cont2xH0_fm_sing_sc", full.names = T), 
         .f = function(file) {
           dt <- readRDS(file)
           do.check(dt)%>%missing.desc.adj(do.adj = 15)
         })%>%
  dplyr::mutate(method = "fm", do = 0.15, hyp = "H0")

saveRDS(do.check.do15, "cluster/out/overall/do.check.fm.15.rds")


h0.sing.do15 <-
  map_df(list.files("cluster/out/fm/2xcont/do15/","cont2xH0_fm_sing_sc", full.names = T), 
         .f = function(file) {
           dt <- readRDS(file)
           h0.sing.sum(dt)%>%missing.desc.adj(do.adj = 15)
         })%>%
  dplyr::mutate(method = "fm")


h0.sing.pcheck.mcar<-
  pcheck.cca(h0.sing.do15,  miss.type = "mcar")

h0.sing.pcheck.mar<-
  pcheck.cca(h0.sing.do15,  miss.type = "mar")

h0.sing.pcheck.mnar<-
  pcheck.cca(h0.sing.do15,  miss.type = "mnar")

saveRDS(h0.sing.do15, "cluster/out/overall/h0.sing.fm.15.rds")

#### MICE ####

h0.mice.do15 <-
  map_df(list.files("cluster/out/fm/2xcont/do15/", "cont2xH0_fm_mice", full.names = T), 
         .f = function(file) {
           df <- readRDS(file)
           h0.mice.sum(df)
         })%>%
  dplyr::mutate(method = "fm", N = num.n.mi, M = num.m.mi)

saveRDS(h0.mice.do15, "cluster/out/overall/h0.mice.fm.15.rds")



##################
#### do = 10% ####
##################

x1.sc21.sing.do10 <- readRDS("cluster/out/fm/2xcont/do10/cont2xH0_fm_sing_sc21_do10_param1.rds")
x1.sc19.sing.do10 <- readRDS("cluster/out/fm/2xcont/do10/cont2xH0_fm_sing_sc19_do10_param1.rds")
x1.sc17.sing.do10 <- readRDS("cluster/out/fm/2xcont/do10/cont2xH0_fm_sing_sc17_do10_param1.rds")
x1.sc2.sing.do10 <- readRDS("cluster/out/fm/2xcont/do10/cont2xH0_fm_sing_sc2_do10_param1.rds")
x1.sc4.sing.do10 <- readRDS("cluster/out/fm/2xcont/do10/cont2xH0_fm_sing_sc4_do10_param1.rds")
x1.sc6.sing.do10 <- readRDS("cluster/out/fm/2xcont/do10/cont2xH0_fm_sing_sc6_do10_param1.rds")
x1.sc23.sing.do10 <- readRDS("cluster/out/fm/2xcont/do10/cont2xH0_fm_sing_sc23_do10_param1.rds")
x1.sc25.sing.do10 <- readRDS("cluster/out/fm/2xcont/do10/cont2xH0_fm_sing_sc25_do10_param1.rds")
x1.sc26.sing.do10 <- readRDS("cluster/out/fm/2xcont/do10/cont2xH0_fm_sing_sc26_do10_param1.rds")

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
  dplyr::mutate(method = "fm")

full.type1 <- readRDS("cluster/out/overall/full.type1.fm.rds")

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
  dplyr::mutate(method = "fm", do = 0.10, hyp = "H0")

saveRDS(do.check.do10, "cluster/out/overall/do.check.fm.10.rds")

#h0.sing.sum(x1.sc4.sing.do10)%>%missing.desc.adj(do.adj = 10)%>%filter(strategy=="cca")


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
  dplyr::mutate(method = "fm")

saveRDS(h0.sing.do10, "cluster/out/overall/h0.sing.fm.10.rds")


### MICE ###
h0.mice.do10 <-
  map_df(list.files("cluster/out/fm/2xcont/do10/", "cont2xH0_fm_mice", full.names = T), 
         .f = function(file) {
           df <- readRDS(file)
           h0.mice.sum(df)
         })%>%
  dplyr::mutate(method = "fm", N = num.n.mi, M = num.m.mi)

saveRDS(h0.mice.do10, "cluster/out/overall/h0.mice.fm.10.rds")


##################
#### do = 5% ####
##################


ll <- c(2,4,6,17,19,21,23,25,26)

full.type1.do5 <-
  map_df(ll, 
         .f = function(sc) {
           dt <- readRDS(list.files("cluster/out/fm/2xcont/do5/", paste0("cont2xH0_fm_sing_sc", sc, "_"), full.names = T))
           full.check(dt, sc)
         })%>%
  dplyr::mutate(method = "fm")

full.type1 <- readRDS("cluster/out/overall/full.type1.fm.rds")

#compare full datasets between simulations for do=20% and do=15%, should be exactly the same
compare::compare(full.type1%>%arrange(scenario.id)%>%
                   filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26)),
                 full.type1.do5%>%arrange(scenario.id))

do.check.do5 <-
  map_df(list.files("cluster/out/fm/2xcont/do5/","cont2xH0_fm_sing_sc", full.names = T), 
         .f = function(file) {
           dt <- readRDS(file)
           do.check(dt)
         })%>%
  dplyr::mutate(method = "fm", do = 0.05, hyp = "H0")

saveRDS(do.check.do5, "cluster/out/overall/do.check.fm.5.rds")


h0.sing.do5 <-
  map_df(list.files("cluster/out/fm/2xcont/do5/","cont2xH0_fm_sing_sc", full.names = T), 
         .f = function(file) {
           dt <- readRDS(file)
           h0.sing.sum(dt)
         })%>%
  dplyr::mutate(method = "fm")

h0.sing.do5<-
  h0.sing.do5%>%
  dplyr::mutate(missing.desc = ifelse(is.na(missing.desc)==T, "  0%", missing.desc))

h0.sing.pcheck.mcar<-
  pcheck.cca(h0.sing.do5,  miss.type = "mcar")

h0.sing.pcheck.mar<-
  pcheck.cca(h0.sing.do5,  miss.type = "mar")

h0.sing.pcheck.mnar<-
  pcheck.cca(h0.sing.do5,  miss.type = "mnar")

saveRDS(h0.sing.do5, "cluster/out/overall/h0.sing.fm.5.rds")


### MICE ###


x1.sc25.mice.do5.condor <- readRDS("cluster/out/fm/2xcont/do5/cont2xH0_fm_mice_sc25_do5_param1.rds")
x1.sc25.mice.do5.local <- readRDS("cluster/out/fm/2xcont/do5/cont2xH0_fm_mice_sc25_do5_param1_local.rds")
x1.sc25.mice.do5 <- append(x1.sc25.mice.do5.condor, x1.sc25.mice.do5.local)
remove(x1.sc25.mice.do5.condor, x1.sc25.mice.do5.local)


x1.sc26.mice.do5.condor <- readRDS("cluster/out/fm/2xcont/do5/cont2xH0_fm_mice_sc26_do5_param1.rds")
x1.sc26.mice.do5.local <- readRDS("cluster/out/fm/2xcont/do5/cont2xH0_fm_mice_sc26_do5_param1_local.rds")
x1.sc26.mice.do5 <- append(x1.sc26.mice.do5.condor, x1.sc26.mice.do5.local)
remove(x1.sc26.mice.do5.condor, x1.sc26.mice.do5.local)

ll <- c(2,4,6,17,19,21,23)

h0.mice.do5 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/fm/2xcont/do5/", paste0("cont2xH0_fm_mice_sc", sc, "_"), full.names = T))
           h0.mice.sum(df)
         })%>%
  bind_rows(
    h0.mice.sum(x1.sc25.mice.do5),
    h0.mice.sum(x1.sc26.mice.do5)
  )%>%
  dplyr::mutate(method = "fm", N = num.n.mi, M = num.m.mi)

saveRDS(h0.mice.do5, "cluster/out/overall/h0.mice.fm.5.rds")

