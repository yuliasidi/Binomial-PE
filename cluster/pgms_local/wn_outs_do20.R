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


################################################
# Empirical type-I error - Fully observed data #
################################################

#check p_C, p_T and type1/power for full data
ll <- seq(1,30,1)

full.type1 <-
  map_df(ll, 
         .f = function(sc) {
           dt <- readRDS(list.files("cluster/out/wn/2xcont/", paste0("cont2xH0_wn_sing_sc", sc, "_"), full.names = T))
           full.check(dt, sc)
         })%>%
  dplyr::mutate(method = "wn")

full.type1%>%
  dplyr::mutate(p_C.check = round(C_phat,3) - p_C,
                M2.check = round(C_phat - T_phat - M2, 3))

saveRDS(full.type1, "cluster/out/overall/full.type1.wn.rds")

################################################
# Empirical power - Fully observed data #
################################################

#check power
ll <- seq(1,30,1)

full.power <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/wn/2xcont/", 
                                    sprintf("cont2xH1_wn_sing_sc%s_do20_param1.rds", sc), 
                                    full.names = T))
           full.check(df, sc)
         })%>%
  dplyr::mutate(method = "wn")

saveRDS(full.power%>%
          dplyr::arrange(scenario.id), "cluster/out/overall/full.power.wn.rds")

#########################################
# Check do rates, data under H0, DO=20% #
########################################
do.check.do20 <-
  map_df(list.files("cluster/out/wn/2xcont/","cont2xH0_wn_sing_sc", full.names = T), 
         .f = function(file) {
           dt <- readRDS(file)
           do.check(dt)
         })%>%
  dplyr::mutate(method = "wn", do = 0.20, hyp = "H0")


saveRDS(do.check.do20, "cluster/out/overall/do.check.wn.20.rds")

#type-I error follow single value imputation
h0.sing.do20 <-
  map_df(list.files("cluster/out/wn/2xcont/","cont2xH0_wn_sing_sc", full.names = T), 
         .f = function(file) {
           dt <- readRDS(file)
           h0.sing.sum(dt)
         })%>%
  dplyr::mutate(method = "wn")

h0.sing.pcheck.mcar<-
  pcheck.cca(h0.sing.do20,  miss.type = "mcar")

h0.sing.pcheck.mar<-
  pcheck.cca(h0.sing.do20,  miss.type = "mar")
  
h0.sing.pcheck.mnar<-
  pcheck.cca(h0.sing.do20,  miss.type = "mnar")

saveRDS(h0.sing.do20, "cluster/out/overall/h0.sing.wn.20.rds")


#########################################
# Check do rates, data under H1, DO=20% #
#########################################

ll <- seq(1,30,1)

do.check.do20.h1 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/wn/2xcont/", 
                                    sprintf("cont2xH1_wn_sing_sc%s_do20_param1.rds", sc), 
                                    full.names = T))
           do.check(df)
         })%>%
  dplyr::mutate(method = "wn", do = 0.2, hyp = "H1")

saveRDS(do.check.do20.h1%>%
          dplyr::arrange(scenario.id), "cluster/out/overall/do.check.wn.20.h1.rds")

##########################################################################
# Empirical type-I error - Incomplete, MICE imputation strategy, DO=20%  #
##########################################################################

ll <- c(seq(1,20,1), seq(22,30,1))

x1.sc21.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc21_do20_param1.rds")

h0.mice.20 <-
  bind_rows(h0.mice.sum.wn(x1.sc21.mice)%>%
             dplyr::filter(k.C.spec!="normal(1.35, 0.05)"),
          map_df(ll, 
                 .f = function(sc) {
                   df <- readRDS(list.files("cluster/out/wn/2xcont/", 
                                            sprintf("cont2xH0_wn_mice_sc%s_do20_param1.rds", sc), 
                                            full.names = T))
                   h0.mice.sum.wn(df)
                 })
)%>%
  dplyr::mutate(method = "wn", N = num.n.mi, M = num.m.mi)

saveRDS(h0.mice.20, "cluster/out/overall/h0.mice.wn.20.rds")

#####################################################################
# Empirical power - Incomplete, single imputation strategy, DO=20%  #
#####################################################################
ll <- seq(1,30,1)

power.sing.20 <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/wn/2xcont/", 
                                    sprintf("cont2xH1_wn_sing_sc%s_do20_param1.rds", sc), 
                                    full.names = T))
           h0.sing.sum(df)%>%
             dplyr::select(-mean.bias)%>%
             dplyr::rename(power=type1)
         })%>%
  dplyr::mutate(method = "wn")

saveRDS(power.sing.20, "cluster/out/overall/h1.sing.wn.20.rds")


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

