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

#######################################################
# Check full data under H0, DO=15% vs previous result #
#######################################################

ll <- seq(1,30,1)

full.type1.do15 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/wald/2xcont/do15", 
                                    sprintf("cont2xH0_wald_sing_sc%s_do15_param1.rds", sc), 
                                    full.names = T))
           full.check(df, sc)
         })%>%
  dplyr::mutate(method = "wald")

full.type1 <- readRDS("cluster/out/overall/full.type1.wald.rds")

#compare full datasets between simulations for do=20% and do=15%, should be exactly the same
compare::compare(full.type1%>%arrange(scenario.id),
                 full.type1.do15%>%arrange(scenario.id))
                 
#########################################
# Check do rates, data under H0, DO=15% #
########################################

ll <- seq(1,30,1)

do.check.do15 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/wald/2xcont/do15", 
                                    sprintf("cont2xH0_wald_sing_sc%s_do15_param1.rds", sc), 
                                    full.names = T))
           do.check(df)%>%missing.desc.adj(do.adj = 15)
         })%>%
  dplyr::mutate(method = "wald", do = 0.15, hyp = "H0")

saveRDS(do.check.do15%>%
          dplyr::arrange(scenario.id), "cluster/out/overall/do.check.wald.15.rds")


#########################################
# Check do rates, data under H1, DO=15% #
#########################################

ll <- seq(1,30,1)

do.check.do15.h1 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/wald/2xcont/do15", 
                                    sprintf("cont2xH1_wald_sing_sc%s_do15_param1.rds", sc), 
                                    full.names = T))
           do.check(df)%>%missing.desc.adj(do.adj = 15)
         })%>%
  dplyr::mutate(method = "wald", do = 0.15, hyp = "H1")

saveRDS(do.check.do15.h1%>%
          dplyr::arrange(scenario.id), "cluster/out/overall/do.check.wald.h1.15.rds")


##########################################################################
# Empirical type-I error - Incomplete, single imputation strategy, DO=15%#
##########################################################################

ll <- seq(1,30,1)

h0.sing <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/wald/2xcont/do15", 
                                     sprintf("cont2xH0_wald_sing_sc%s_do15_param1.rds", sc), 
                                     full.names = T))
           h0.sing.sum(df)%>%missing.desc.adj(do.adj = 15)
         })%>%
  dplyr::mutate(method = "wald")


h0.sing.pcheck.mcar<-
  pcheck.cca(h0.sing,  miss.type = "mcar")

h0.sing.pcheck.mar<-
  pcheck.cca(h0.sing,  miss.type = "mar")

h0.sing.pcheck.mnar<-
  pcheck.cca(h0.sing,  miss.type = "mnar")

saveRDS(h0.sing, "cluster/out/overall/h0.sing.wald.15.rds")

##########################################################################
# Empirical type-I error - Incomplete, MICE imputation strategy, DO=15%  #
##########################################################################

ll <- seq(1,30,1)

h0.mice <-
    map_df(ll, 
           .f = function(sc) {
             df <- readRDS(list.files("cluster/out/wald/2xcont/do15", 
                                      sprintf("cont2xH0_wald_mice_sc%s_do15", sc), 
                                      full.names = T))
             h0.mice.sum(df)
           })%>%
  dplyr::mutate(method = "wald", N = num.n.mi, M = num.m.mi)

saveRDS(h0.mice, "cluster/out/overall/h0.mice.wald.15.rds")

#####################################################################
# Empirical power - Incomplete, single imputation strategy, DO=15%  #
#####################################################################
ll <- seq(1,30,1)

power.sing.15 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/wald/2xcont/do15", 
                                    sprintf("cont2xH1_wald_sing_sc%s_do15_param1.rds", sc), 
                                    full.names = T))
           h0.sing.sum(df)%>%
             dplyr::select(-mean.bias)%>%
             dplyr::rename(power=type1)
         })%>%
  dplyr::mutate(method = "wald")

saveRDS(power.sing.15, "cluster/out/overall/h1.sing.wald.15.rds")


###################################################################
# Empirical power - Incomplete, MICE imputation strategy, DO=15%  #
###################################################################
ll <- seq(1,30,1)

power.mice.15 <-
  map_df(ll, 
       .f = function(sc) {
         df <- readRDS(list.files("cluster/out/wald/2xcont/do15", 
                                  sprintf("cont2xH1_wald_mice_sc%s_do15_param1.rds", sc), 
                                  full.names = T))
         h0.mice.sum(df)%>%
           dplyr::select(-mean.bias)%>%
           dplyr::rename(power=type1)
       })%>%
  dplyr::mutate(method = "wald", N = num.n.mi, M = num.m.mi)

saveRDS(power.mice.15, "cluster/out/overall/h1.mice.wald.15.rds")


