library(dplyr)
library(ggplot2)
library(tidyr)

ss <- readRDS("cluster/ss.bounds.rds")

ss.plot <-
  ss%>%
  ggplot(aes(x=p_C, y = n.arm, fill = method)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  theme_bw() +
  facet_wrap(~M2) +
  theme(legend.position = "bottom") +
  labs(x = "Event probability in control group",
       y = "N per arm",
       fill = "Method")


pdf("cluster/checks/ss_per_method.pdf")
ss.plot
dev.off()

