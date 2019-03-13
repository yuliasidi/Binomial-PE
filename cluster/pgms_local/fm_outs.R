library(dplyr)
library(ggplot2)
library(plotly)


t<-purrr::map_df(list.files(path = "cluster/out/fm", pattern = "p30_fm15_10", full.names = TRUE), readRDS)


t1 <- t%>%
  dplyr::group_by(strategy, missing)%>%
  dplyr::summarise(type1=mean(reject.h0))

t1.p <- t1%>%ggplot(aes(x = missing, y = type1)) +
  geom_point(aes(color = strategy)) +
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) +
  scale_y_continuous(breaks = c(seq(0,0.05, 0.025), 0.075, seq(0.1, 1, 0.1)))

t2.pp <- plotly::ggplotly(t1.p)

htmlwidgets::saveWidget(t2.pp, file = "H0_p30_fm15_10.html",selfcontained = TRUE)
