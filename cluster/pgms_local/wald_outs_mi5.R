library(dplyr)
library(purrr)
library(ggplot2)
library(plotly)

source("funs/plot.type1.R")
source("funs/plot.bias.R")
source("funs/plot.power.R")

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
  