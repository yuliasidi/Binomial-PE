library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(plotly)

source("cluster/pgms/init.R")
source("funs/full.check.R")
source("funs/do.check.R")
source("funs/h0.sing.sum.R")
source("funs/h0.mice.sum.wn.R")
source("funs/plot.type1.R")
source("funs/plot.bias.R")
source("funs/plot.power.R")
source("funs/miss.desc.R")

ss <- readRDS("cluster/ss.bounds.rds")
ss <- ss%>%
  dplyr::filter(method == "wn")

x1.sc26.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc26_do20_param1.rds")
x1.sc25.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc25_do20_param1.rds")
x1.sc23.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc23_do20_param1.rds")
x1.sc21.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc21_do20_param1.rds")
x1.sc17.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc17_do20_param1.rds")
x1.sc19.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc19_do20_param1.rds")
x1.sc2.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc2_do20_param1.rds")
x1.sc4.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc4_do20_param1.rds")
x1.sc6.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc6_do20_param1.rds")
x1.sc21.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc21_do20_param1.rds")


#additions
x1.sc27.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc27_do20_param1.rds")
x1.sc22.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc22_do20_param1.rds")
x1.sc18.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc18_do20_param1.rds")
x1.sc16.sing <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_sing_sc16_do20_param1.rds")

#check p_C, p_T and type1/power for full data
full.type1<-
  bind_rows(
    full.check(x1.sc21.sing, 21),
    full.check(x1.sc19.sing, 19),
    full.check(x1.sc17.sing, 17),
    full.check(x1.sc6.sing, 6),
    full.check(x1.sc4.sing, 4),
    full.check(x1.sc2.sing, 2),
    full.check(x1.sc23.sing, 23),
    full.check(x1.sc25.sing, 25),
    full.check(x1.sc26.sing, 26)
  )%>%
  dplyr::mutate(method = "wn")

saveRDS(full.type1, "cluster/out/overall/full.type1.wn.rds")

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
  dplyr::mutate(method = "wn", do = 0.2)

saveRDS(do.check, "cluster/out/overall/do.check.wn.20.rds")

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
  dplyr::mutate(method = "wn")

saveRDS(h0.sing, "cluster/out/overall/h0.sing.wn.20.rds")


#Read and summarise results for MICE
x1.sc2.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc2_do20_param1.rds")
x1.sc4.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc4_do20_param1.rds")

x1.sc6.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc6_do20_param1.rds")
x1.sc19.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc19_do20_param1.rds")
x1.sc21.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc21_do20_param1.rds")



x1.sc17.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc17_do20_param1.rds")
x1.sc26.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc26_do20_param1.rds")
x1.sc23.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc23_do20_param1.rds")
x1.sc25.mice <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc25_do20_param1.rds")



h0.mice <-
  bind_rows(
    h0.mice.sum.wn(x1.sc21.mice)%>%
      dplyr::filter(k.C.spec!="normal(1.35, 0.05)"),
    h0.mice.sum.wn(x1.sc2.mice),
    h0.mice.sum.wn(x1.sc4.mice),
    h0.mice.sum.wn(x1.sc6.mice),
    h0.mice.sum.wn(x1.sc19.mice),
    h0.mice.sum.wn(x1.sc17.mice),
    h0.mice.sum.wn(x1.sc26.mice),
    h0.mice.sum.wn(x1.sc23.mice),
    h0.mice.sum.wn(x1.sc25.mice)
    
    
    
    
  )%>%
  dplyr::mutate(method = "wn", N = num.n.mi, M = num.m.mi)

saveRDS(h0.mice, "cluster/out/overall/h0.mice.wn.20.rds")







full.check(x1.sc22.sing, 22)
do.check(x1.sc22.sing)

h0.sing.sum(x1.sc22.sing)%>%
  dplyr::filter(strategy=="cca")


h0.mice.sum(x1.sc21.mice)
h0.sing.sum(x1.sc21.bw)

h0.mice.sum(x1.sc17.mice)

h0.sing <- h0.sing%>%
  dplyr::left_join(ss%>%
                     dplyr::select(scenario.id, p_C, M2, n.arm), by = "scenario.id")

h0.sing$missing.desc <- factor(h0.sing$missing.desc,levels=unique(h0.sing$missing.desc)[c(4,3,2,1,5,6,7,8,9,10)])
h0.sing <- h0.sing%>%
  dplyr::mutate(flabel = sprintf('p[C]: %s, Delta: %s, n: %s',p_C, M2, n.arm))%>%
  left_join(full.type1%>%
              dplyr::select(scenario.id, reject.h0)%>%
              dplyr::rename(type1.comp = reject.h0), by = "scenario.id")

#type1.plot.20.mar.ccamice <-
h0.sing%>%
  filter(strategy%in%c("cca"), missing%in%c("mar1", "mar2", "mar3", "mar4", "mar5", "mar6", "mar7", "mcar"))%>%
  plot.type1(do.val = 0.2, 
             p.title = "Type-I error: Overall drop-out rate of 20%")

#bias.plot.20.mar.ccamice <-
h0.sing%>%
  filter(strategy%in%c("cca"), missing%in%c("mar1", "mar2", "mar3", "mar4", "mar5", "mar6", "mar7", "mcar"))%>%
  plot.bias(do.val = 0.2, 
            p.title = "Bias: Overall drop-out rate of 20%")



#type1.plot.20.mar.ccamice <-
h0.sing%>%
  filter(strategy%in%c("cca"), missing%in%c("mnar1", "mnar2"))%>%
  plot.type1(do.val = 0.2, 
             p.title = "Type-I error: Overall drop-out rate of 20%")

#bias.plot.20.mar.ccamice <-
h0.sing%>%
  filter(strategy%in%c("cca"), missing%in%c("mnar1", "mnar2"))%>%
  plot.bias(do.val = 0.2, 
            p.title = "Bias: Overall drop-out rate of 20%")


# h0.sing%>%
#   filter(strategy%in%c("best"), missing%in%c("mar1", "mar2", "mar3", "mar4", "mar5", "mar6", "mar7", "mcar"))%>%
#   plot.type1(do.val = 0.2, 
#              p.title = "Type-I error: Overall drop-out rate of 20%")
# 
# h0.sing%>%
#   filter(strategy%in%c("best"), missing%in%c("mar1", "mar2", "mar3", "mar4", "mar5", "mar6", "mar7", "mcar"))%>%
#   plot.bias(do.val = 0.2, 
#             p.title = "Bias: Overall drop-out rate of 20%")
# 
# 
# h0.sing%>%
#   filter(strategy%in%c("worst"), missing%in%c("mar1", "mar2", "mar3", "mar4", "mar5", "mar6", "mar7", "mcar"))%>%
#   plot.type1(do.val = 0.2, 
#              p.title = "Type-I error: Overall drop-out rate of 20%")
# 
# h0.sing%>%
#   filter(strategy%in%c("worst"), missing%in%c("mar1", "mar2", "mar3", "mar4", "mar5", "mar6", "mar7", "mcar"))%>%
#   plot.bias(do.val = 0.2, 
#             p.title = "Bias: Overall drop-out rate of 20%")







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
