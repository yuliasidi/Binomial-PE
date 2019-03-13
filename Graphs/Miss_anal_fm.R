library(dplyr)
library(ggplot2)
library(plotly)

dt <- readRDS("clusterdata/dtfullfm_4.rds") 
dt4 <- dt%>%select(p_C, M2)


dt <- readRDS("clusterdata/dtfullfm_5.rds") 
dt5 <- dt%>%select(p_C, M2)

dt <- readRDS("clusterdata/dtfullfm_27.rds") 
dt27 <- dt%>%select(p_C, M2)

cca_fm_5_do10.plotH0 <- cca_fm_5_do10%>%
  dplyr::filter(type == "t.H0")%>%
  ggplot2::ggplot(aes(x = missing, y = err, color = strategy)) +
  ggplot2::geom_point(aes(color=strategy)) +
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2) +
  ylab("Empirical Type-I")

plotly::ggplotly(cca_fm_5_do10.plotH0)


cca_fm_5_do10.plotH1 <- cca_fm_5_do10%>%
  dplyr::filter(type == "t.H1")%>%
  ggplot2::ggplot(aes(x = missing, y = err, color = strategy)) +
  ggplot2::geom_point(aes(color=strategy)) +
  ylab("Empirical Power")
plotly::ggplotly(cca_fm_5_do10.plotH1)


cca_fm_27_do10.plotH0 <- cca_fm_27_do10%>%
  dplyr::filter(type == "t.H0")%>%
  ggplot2::ggplot(aes(x = missing, y = err, color = strategy)) +
  ggplot2::geom_point(aes(color=strategy)) +
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2) +
  ylab("Empirical Type-I")

plotly::ggplotly(cca_fm_27_do10.plotH0)


cca_fm_27_do10.plotH1 <- cca_fm_27_do10%>%
  dplyr::filter(type == "t.H1")%>%
  ggplot2::ggplot(aes(x = missing, y = err, color = strategy)) +
  ggplot2::geom_point(aes(color=strategy)) +
  ylab("Empirical Power") +
  geom_hline(yintercept = 0.9, linetype = 2)
plotly::ggplotly(cca_fm_27_do10.plotH1)

cca_fm_4_do10.plotH0 <- cca_fm_4_do10%>%
  dplyr::filter(type == "t.H0")%>%
  ggplot2::ggplot(aes(x = missing, y = err, color = strategy)) +
  ggplot2::geom_point(aes(color=strategy)) +
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2) +
  ylab("Empirical Type-I")

plotly::ggplotly(cca_fm_4_do10.plotH0)


cca_fm_4_do10.plotH1 <- cca_fm_4_do10%>%
  dplyr::filter(type == "t.H1")%>%
  ggplot2::ggplot(aes(x = missing, y = err, color = strategy)) +
  ggplot2::geom_point(aes(color=strategy)) +
  ylab("Empirical Power")
plotly::ggplotly(cca_fm_4_do10.plotH1)

