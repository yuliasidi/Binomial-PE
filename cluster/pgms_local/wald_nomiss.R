library(dplyr)
library(purrr)
library(ggplot2)
library(plotly)

source("funs/plot.type1.R")


#type-I error for 5000 simulations vs 10000 simulations
t29 <- readRDS("cluster/out/wald/nomiss/outH0_p30_wald29.rds")
t21 <- readRDS("cluster/out/wald/nomiss/outH0_p30_wald21.rds")
t15 <- readRDS("cluster/out/wald/nomiss/outH0_p30_wald15.rds")
t1 <- readRDS("cluster/out/wald/nomiss/outH0_p30_wald1.rds")

bind_rows(t29)%>%
  dplyr::summarise(type1=mean(reject.h0))

bind_rows(t29[1:5000])%>%
  dplyr::summarise(type1=mean(reject.h0))

bind_rows(t15)%>%
  dplyr::summarise(type1=mean(reject.h0))

bind_rows(t15[1:5000])%>%
  dplyr::summarise(type1=mean(reject.h0))

bind_rows(t21)%>%
  dplyr::summarise(type1=mean(reject.h0))

bind_rows(t21[1:5000])%>%
  dplyr::summarise(type1=mean(reject.h0))

bind_rows(t1)%>%
  dplyr::summarise(type1=mean(reject.h0))

bind_rows(t1[1:5000])%>%
  dplyr::summarise(type1=mean(reject.h0))


#power for 5000 simulations vs 10000 simulations
t29.1 <- readRDS("cluster/out/wald/nomiss/outH1_p30_wald29.rds")
t21.1 <- readRDS("cluster/out/wald/nomiss/outH1_p30_wald21.rds")
t15.1 <- readRDS("cluster/out/wald/nomiss/outH1_p30_wald15.rds")
t1.1 <- readRDS("cluster/out/wald/nomiss/outH1_p30_wald1.rds")

bind_rows(t29.1)%>%
  dplyr::summarise(power=mean(reject.h0))

bind_rows(t29.1[1:5000])%>%
  dplyr::summarise(power=mean(reject.h0))

bind_rows(t15.1)%>%
  dplyr::summarise(power=mean(reject.h0))

bind_rows(t15.1[1:5000])%>%
  dplyr::summarise(power=mean(reject.h0))

bind_rows(t21.1)%>%
  dplyr::summarise(power=mean(reject.h0))

bind_rows(t21.1[1:5000])%>%
  dplyr::summarise(power=mean(reject.h0))

bind_rows(t1.1)%>%
  dplyr::summarise(power=mean(reject.h0))

bind_rows(t1.1[1:5000])%>%
  dplyr::summarise(power=mean(reject.h0))



ss.bounds <- readRDS("cluster/ss.bounds.rds")

ss.bounds <- ss.bounds%>%
  dplyr::filter(method=="wald")

ff.n <- expand.grid(scenario = seq(1,30,2), do = c(10,20))

ff.n1 <- as.list(ff.n%>%filter(scenario%in%c(21), do == 20))
summ.type1 <- pmap_df(ff.n1, .f = function(scenario, do){
  t <-readRDS(sprintf("cluster/out/wald/outH0_p30_wald%d_%d.rds", scenario, do))
  
  t1 <- t%>%
    dplyr::bind_rows()%>%
    dplyr::group_by(scenario.id, strategy, missing, do)%>%
    dplyr::summarise(type1=mean(reject.h0))
  
  return(t1)
  
})
