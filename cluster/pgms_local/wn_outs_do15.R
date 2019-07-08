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
source("funs/pcheck.cca.R") 

ss <- readRDS("cluster/ss.bounds.rds")
ss <- ss%>%
  dplyr::filter(method == "wn")


#######################################################
# Check full data under H0, DO=15% vs previous result #
#######################################################

ll <- seq(1,30,1)

full.type1.do15 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/wn/2xcont/do15", 
                                    sprintf("cont2xH0_wn_sing_sc%s_do15_param1.rds", sc), 
                                    full.names = T))
           full.check(df, sc)
         })%>%
  dplyr::mutate(method = "wn")

full.type1 <- readRDS("cluster/out/overall/full.type1.wn.rds")

#compare full datasets between simulations for do=20% and do=15%, should be exactly the same
compare::compare(full.type1%>%arrange(scenario.id),
                 full.type1.do15%>%arrange(scenario.id))


#########################################
# Check do rates, data under H0, DO=15% #
########################################
do.check.do15 <-
  map_df(list.files("cluster/out/wn/2xcont/do15","cont2xH0_wn_sing_sc", full.names = T), 
         .f = function(file) {
           dt <- readRDS(file)
           do.check(dt)%>%missing.desc.adj(do.adj = 15)
         })%>%
  dplyr::mutate(method = "wn", do = 0.15, hyp = "H0")


saveRDS(do.check.do15, "cluster/out/overall/do.check.wn.15.rds")

##########################################################################
# Empirical type-I error - Incomplete, single imputation strategy, DO=15%#
##########################################################################

h0.sing.do15 <-
  map_df(list.files("cluster/out/wn/2xcont/do15","cont2xH0_wn_sing_sc", full.names = T), 
         .f = function(file) {
           dt <- readRDS(file)
           h0.sing.sum(dt)%>%missing.desc.adj(do.adj = 15)
         })%>%
  dplyr::mutate(method = "wn")

h0.sing.pcheck.mcar<-
  pcheck.cca(h0.sing.do15,  miss.type = "mcar")

h0.sing.pcheck.mar<-
  pcheck.cca(h0.sing.do15,  miss.type = "mar")
  
h0.sing.pcheck.mnar<-
  pcheck.cca(h0.sing.do15,  miss.type = "mnar")

saveRDS(h0.sing.do15, "cluster/out/overall/h0.sing.wn.15.rds")


##########################################################################
# Empirical type-I error - Incomplete, MICE imputation strategy, DO=15%  #
##########################################################################

ll <- seq(1,30,1)

h0.mice.15 <-
          map_df(ll, 
                 .f = function(sc) {
                   df <- readRDS(list.files("cluster/out/wn/2xcont/do15", 
                                            sprintf("cont2xH0_wn_mice_sc%s_do15_param1.rds", sc), 
                                            full.names = T))
                   h0.mice.sum.wn(df)%>%missing.desc.adj(do.adj = 15)
                 })%>%
  dplyr::mutate(method = "wn", N = num.n.mi, M = num.m.mi)

saveRDS(h0.mice.15, "cluster/out/overall/h0.mice.wn.15.rds")


##################
#### do = 10% ####
##################

ll <- c(2,4,6,17,19,21,23,25,26)

full.type1.do10 <-
  map_df(ll, 
         .f = function(sc) {
           dt <- readRDS(list.files("cluster/out/wn/2xcont/do10/", paste0("cont2xH0_wn_sing_sc", sc, "_"), full.names = T))
           full.check(dt, sc)
         })%>%
  dplyr::mutate(method = "wn")

full.type1 <- readRDS("cluster/out/overall/full.type1.wn.rds")

#compare full datasets between simulations for do=20% and do=15%, should be exactly the same
compare::compare(full.type1%>%arrange(scenario.id)%>%
                   filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26)),
                 full.type1.do10%>%arrange(scenario.id))

do.check.do10 <-
  map_df(list.files("cluster/out/wn/2xcont/do10/","cont2xH0_wn_sing_sc", full.names = T), 
         .f = function(file) {
           dt <- readRDS(file)
           do.check(dt)%>%missing.desc.adj(do.adj = 10)
         })%>%
  dplyr::mutate(method = "wn", do = 0.10, hyp = "H0")

saveRDS(do.check.do10, "cluster/out/overall/do.check.wn.10.rds")

h0.sing.sum(x1.sc26.sing.do10)%>%missing.desc.adj(do.adj = 10)%>%filter(strategy=="cca")

h0.sing.do10 <-
  map_df(list.files("cluster/out/wn/2xcont/do10/","cont2xH0_wn_sing_sc", full.names = T), 
         .f = function(file) {
           dt <- readRDS(file)
           h0.sing.sum(dt)%>%missing.desc.adj(do.adj = 10)
         })%>%
  dplyr::mutate(method = "wn")


saveRDS(h0.sing.do10, "cluster/out/overall/h0.sing.wn.10.rds")


### MICE ###

h0.mice.do10 <-
  map_df(list.files("cluster/out/wn/2xcont/do10/", "cont2xH0_wn_mice", full.names = T), 
         .f = function(file) {
           df <- readRDS(file)
           h0.mice.sum.wn(df)
         })%>%
  dplyr::mutate(method = "wn", N = num.n.mi, M = num.m.mi)

saveRDS(h0.mice.do10, "cluster/out/overall/h0.mice.wn.10.rds")

##################
#### do = 5% ####
##################

ll <- c(2,4,6,17,19,21,23,25,26)

full.type1.do5 <-
  map_df(ll, 
         .f = function(sc) {
           dt <- readRDS(list.files("cluster/out/wn/2xcont/do5/", paste0("cont2xH0_wn_sing_sc", sc, "_"), full.names = T))
           full.check(dt, sc)
         })%>%
  dplyr::mutate(method = "wn")

full.type1 <- readRDS("cluster/out/overall/full.type1.wn.rds")

#compare full datasets between simulations for do=20% and do=15%, should be exactly the same
compare::compare(full.type1%>%arrange(scenario.id)%>%
                   filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26)),
                 full.type1.do5%>%arrange(scenario.id))

do.check.do5 <-
  map_df(list.files("cluster/out/wn/2xcont/do5/","cont2xH0_wn_sing_sc", full.names = T), 
         .f = function(file) {
           dt <- readRDS(file)
           do.check(dt)
         })%>%
  dplyr::mutate(method = "wn", do = 0.05, hyp = "H0")

saveRDS(do.check.do5, "cluster/out/overall/do.check.wn.5.rds")

#h0.sing.sum(x1.sc26.sing.do10)%>%missing.desc.adj(do.adj = 10)%>%filter(strategy=="cca")

h0.sing.do5 <-
  map_df(list.files("cluster/out/wn/2xcont/do5/","cont2xH0_wn_sing_sc", full.names = T), 
         .f = function(file) {
           dt <- readRDS(file)
           h0.sing.sum(dt)
         })%>%
  dplyr::mutate(method = "wn")

h0.sing.do5<-
  h0.sing.do5%>%
  dplyr::mutate(missing.desc = ifelse(is.na(missing.desc)==T, "  0%", missing.desc))

h0.sing.pcheck.mcar<-
  pcheck.cca(h0.sing.do5,  miss.type = "mcar")

h0.sing.pcheck.mar<-
  pcheck.cca(h0.sing.do5,  miss.type = "mar")

h0.sing.pcheck.mnar<-
  pcheck.cca(h0.sing.do5,  miss.type = "mnar")

saveRDS(h0.sing.do5, "cluster/out/overall/h0.sing.wn.5.rds")


### MICE ###


x1.sc25.mice.do5.1 <- readRDS("cluster/out/wn/2xcont/do5/cont2xH0_wn_mice_sc25_do5_param1.rds")
x1.sc25.mice.do5.2 <- readRDS("cluster/out/wn/2xcont/do5/cont2xH0_wn_mice_sc25_do5_param12.rds")
x1.sc25.mice.do5.local <- readRDS("cluster/out/wn/2xcont/do5/cont2xH0_wn_mice_sc25_do5_param11_local.rds")
x1.sc25.mice.do5 <- append(x1.sc25.mice.do5.1,append(x1.sc25.mice.do5.2, x1.sc25.mice.do5.local))
remove(x1.sc25.mice.do5.1, x1.sc25.mice.do5.2, x1.sc25.mice.do5.local)



x1.sc26.mice.do5.condor <- readRDS("cluster/out/wn/2xcont/do5/cont2xH0_wn_mice_sc26_do5_param1.rds")
x1.sc26.mice.do5.local <- readRDS("cluster/out/wn/2xcont/do5/cont2xH0_wn_mice_sc26_do5_param11_local.rds")
x1.sc26.mice.do5 <- append(x1.sc26.mice.do5.condor, x1.sc26.mice.do5.local)
remove(x1.sc26.mice.do5.condor, x1.sc26.mice.do5.local)

ll <- c(2,4,6,17,19,21,23)

h0.mice.do5 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/wn/2xcont/do5/", paste0("cont2xH0_wn_mice_sc", sc, "_"), full.names = T))
           h0.mice.sum.wn(df)
         })%>%
  bind_rows(
    h0.mice.sum.wn(x1.sc25.mice.do5),
    h0.mice.sum.wn(x1.sc26.mice.do5)
  )%>%
  dplyr::mutate(method = "wn", N = num.n.mi, M = num.m.mi)

saveRDS(h0.mice.do5, "cluster/out/overall/h0.mice.wn.5.rds")
