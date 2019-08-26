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

#######################################################
# Check full data under H1, DO=10% vs previous result #
#######################################################

ll <- seq(1,30,1)

full.power.do10 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/fm/2xcont/do10", 
                                    sprintf("cont2xH1_fm_sing_sc%s_do10_param1.rds", sc), 
                                    full.names = T))
           full.check(df, sc)
         })%>%
  dplyr::mutate(method = "fm")

full.power <- readRDS("cluster/out/overall/full.power.fm.rds")

#compare full datasets between simulations for do=20% and do=10%, should be exactly the same
compare::compare(full.power.do10%>%arrange(scenario.id),
                 full.power%>%arrange(scenario.id))


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

do.check.do10.h1 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/fm/2xcont/do10", 
                                    sprintf("cont2xH1_fm_sing_sc%s_do10_param1.rds", sc), 
                                    full.names = T))
           do.check(df)%>%missing.desc.adj(do.adj = 10)
         })%>%
  dplyr::mutate(method = "fm", do = 0.10, hyp = "H1")

saveRDS(do.check.do10.h1%>%
          dplyr::arrange(scenario.id), "cluster/out/overall/checks/do.check.fm.10.h1.rds")

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


#####################################################################
# Empirical power - Incomplete, single imputation strategy, DO=10%  #
#####################################################################
ll <- seq(1,30,1)

power.sing.10 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/fm/2xcont/do10", 
                                    sprintf("cont2xH1_fm_sing_sc%s_do10_param1.rds", sc), 
                                    full.names = T))
           h0.sing.sum(df)%>%
             dplyr::select(-mean.bias)%>%
             dplyr::rename(power=type1)
         })%>%
  dplyr::mutate(method = "fm")

saveRDS(power.sing.10, "cluster/out/overall/h1.sing.fm.10.rds")


###################################################################
# Empirical power - Incomplete, MICE imputation strategy, DO=10%  # 
###################################################################
ll <- seq(1,30,1)

power.mice.10 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/fm/2xcont/do10", 
                                    sprintf("cont2xH1_fm_mice_sc%s_do10_param1.rds", sc), 
                                    full.names = T))
           h0.mice.sum(df)%>%
             dplyr::select(-mean.bias)%>%
             dplyr::rename(power=type1)
         })%>%
  dplyr::mutate(method = "fm", N = num.n.mi, M = num.m.mi)

saveRDS(power.mice.10, "cluster/out/overall/h1.mice.fm.10.rds")






