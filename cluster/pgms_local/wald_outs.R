library(dplyr)
library(purrr)
library(ggplot2)
library(plotly)

ff.n <- expand.grid(scenario = seq(1,30,2), do = c(10,20))

ff.n1 <- as.list(ff.n%>%filter(scenario==29))
summ.type1 <- pmap_df(ff.n1, .f = function(scenario, do){
  t <-readRDS(sprintf("cluster/out/wald/outH0_p30_wald%d_%d.rds", scenario, do))

  t1 <- t%>%
    dplyr::bind_rows()%>%
    dplyr::group_by(strategy, missing, do)%>%
    dplyr::summarise(type1=mean(reject.h0))
  
  return(t1)
  
})


summ.type1.p <- summ.type1%>%
  dplyr::filter(strategy!="mice m=10")%>%
  ggplot(aes(x = missing, y = type1)) +
  geom_point(aes(color = strategy)) +
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) +
  scale_y_continuous(breaks = c(seq(0,0.05, 0.025), 0.075, seq(0.1, 1, 0.1))) +
  facet_wrap(~do)

t2.pp <- plotly::ggplotly(summ.type1.p)

htmlwidgets::saveWidget(t2.pp, file = "H0_p30_wald29.html",selfcontained = TRUE)


t <- readRDS("cluster/out/wald/outH1_p30_wald29_10.rds")

t1<- bind_rows(t)

t2 <- t1%>%
  dplyr::group_by(strategy, missing)%>%
  dplyr::summarise(power=mean(reject.h0))

t2.p <- t2%>%ggplot(aes(x = missing, y = power)) +
  geom_point(aes(color = strategy)) +
  geom_hline(yintercept=0.9,
             linetype=2) +
  scale_y_continuous(breaks = c(seq(0, 0.7, 0.1), seq(0.7, 1, 0.05)))

t2.pp <- plotly::ggplotly(t2.p)

htmlwidgets::saveWidget(t2.pp, file = "H1_p30_wald29_10.html",selfcontained = TRUE)


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
