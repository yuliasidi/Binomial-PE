library(dplyr)
library(purrr)
library(ggplot2)
library(plotly)

source("funs/plot.type1.R")

ss.bounds <- readRDS("cluster/ss.bounds.rds")

ss.bounds <- ss.bounds%>%
  dplyr::filter(method=="wald")

ff.n <- expand.grid(scenario = seq(1,30,2), do = c(10,20))

ff.n1 <- as.list(ff.n%>%filter(scenario%in%c(19,21,23,25,27,29)))
summ.type1 <- pmap_df(ff.n1, .f = function(scenario, do){
  t <-readRDS(sprintf("cluster/out/wald/outH0_p30_wald%d_%d.rds", scenario, do))

  t1 <- t%>%
    dplyr::bind_rows()%>%
    dplyr::group_by(scenario.id, strategy, missing, do)%>%
    dplyr::summarise(type1=mean(reject.h0))
  
  return(t1)
  
})

summ.type1<- summ.type1%>%
  left_join(ss.bounds, by = "scenario.id")%>%
  dplyr::mutate(sc=sprintf("M2=%s, p=%s, n=%s", M2, p_C, n.arm))
  #dplyr::mutate(f=sprintf("Delta*':%s'*', '*p[C]*': %s'", M2, p_C))


summ.type1.p <- summ.type1%>%
  ggplot(aes(x = missing, y = type1)) +
  geom_point(aes(color = strategy)) +
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) +
  #  scale_y_continuous(breaks = c(seq(0,0.05, 0.025), 0.075, seq(0.1, 1, 0.1))) +
  facet_wrap(do ~ sc, ncol = 5, nrow = 2)

plotly::ggplotly(summ.type1.p)


type1.mi.cca <- summ.type1%>%filter(strategy%in%c("cca", "mice m=20"))

mcar.type1  <- plot.type1(summ.type1, "mcar",  "MCAR")
mar1.type1  <- plot.type1(summ.type1, "mar1",  "MAR: beta_X = 1, beta_trt = 0")
mar2.type1  <- plot.type1(summ.type1, "mar2",  "MAR: beta_X = -1, beta_trt = 0")
mar3.type1  <- plot.type1(summ.type1, "mar3",  "MAR: beta_X = 1, beta_trt = 0.5")
mar4.type1  <- plot.type1(summ.type1, "mar4",  "MAR: beta_X = -1, beta_trt = -0.5")
mar5.type1  <- plot.type1(summ.type1, "mar5",  "MAR: beta_X = -1, beta_trt = 0.5")
mar6.type1  <- plot.type1(summ.type1, "mar6",  "MAR: beta_X = 1, beta_trt = -0.5")
mnar1.type1 <- plot.type1(summ.type1, "mnar1", "MNAR: beta_X = 1, beta_trt = 0, beta_y=0.5")
mnar2.type1 <- plot.type1(summ.type1, "mnar2", "MNAR: beta_X =  1, beta_trt = 0, beta_y=-0.5")


mcar.p1 <- plotly::ggplotly(mcar.type1)
mar1.p1 <- plotly::ggplotly(mar1.type1)
mar2.p1 <-plotly::ggplotly(mar2.type1)
mar3.p1 <-plotly::ggplotly(mar3.type1)
mar4.p1 <-plotly::ggplotly(mar4.type1)
mar5.p1 <-plotly::ggplotly(mar5.type1)
mar6.p1 <-plotly::ggplotly(mar6.type1)
mnar1.p1 <-plotly::ggplotly(mnar1.type1)
mnar2.p1 <-plotly::ggplotly(mnar2.type1)



htmlwidgets::saveWidget(mcar.p1, file = "mcar_wald19_29.html",selfcontained = TRUE)
htmlwidgets::saveWidget(mar1.p1, file = "mar1_wald19_29.html",selfcontained = TRUE)
htmlwidgets::saveWidget(mar2.p1, file = "mar2_wald19_29.html",selfcontained = TRUE)
htmlwidgets::saveWidget(mar3.p1, file = "mar3_wald19_29.html",selfcontained = TRUE)
htmlwidgets::saveWidget(mar4.p1, file = "mar4_wald19_29.html",selfcontained = TRUE)
htmlwidgets::saveWidget(mar5.p1, file = "mar5_wald19_29.html",selfcontained = TRUE)
htmlwidgets::saveWidget(mar6.p1, file = "mar6_wald19_29.html",selfcontained = TRUE)
htmlwidgets::saveWidget(mnar1.p1, file = "mnar1_wald19_29.html",selfcontained = TRUE)
htmlwidgets::saveWidget(mnar2.p1, file = "mnar2_wald19_29.html",selfcontained = TRUE)











htmlwidgets::saveWidget(t2.pp, file = "H0_p30_wald252729.html",selfcontained = TRUE)


summ.power <- pmap_df(ff.n1, .f = function(scenario, do){
  t <-readRDS(sprintf("cluster/out/wald/outH1_p30_wald%d_%d.rds", scenario, do))
  
  t1 <- t%>%
    dplyr::bind_rows()%>%
    dplyr::group_by(scenario.id, strategy, missing, do)%>%
    dplyr::summarise(power=mean(reject.h0))
  
  return(t1)
  
})

summ.power.p <- summ.power%>%
  dplyr::filter(strategy!="mice m=10")%>%
  ggplot(aes(x = missing, y = power)) +
  geom_point(aes(color = strategy)) +
  geom_hline(yintercept=0.9,
             linetype=2) +
  scale_y_continuous(breaks = c(seq(0, 0.7, 0.1), seq(0.7, 1, 0.05))) +
  facet_wrap(~do)

t2.pp <- plotly::ggplotly(summ.power.p)

htmlwidgets::saveWidget(t2.pp, file = "H1_p30_wald29.html",selfcontained = TRUE)


t <- readRDS("cluster/out/wald/nomiss/outH0_p30_wald15.rds")

t1<- bind_rows(t)

t2 <- t1%>%
  dplyr::summarise(type1=mean(reject.h0))

t <- readRDS("cluster/out/wald/nomiss/outH1_p30_wald15.rds")

t1<- bind_rows(t)

t2 <- t1%>%
  dplyr::summarise(type1=mean(reject.h0))


t <- readRDS("cluster/out/wald/nomiss/outH0_p30_wald29.rds")

bind_rows(t)%>%
  dplyr::summarise(type1=mean(reject.h0))

t <- readRDS("cluster/out/wald/nomiss/outH1_p30_wald29.rds")

bind_rows(t)%>%
  dplyr::summarise(power=mean(reject.h0))
