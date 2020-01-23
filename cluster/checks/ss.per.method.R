library(dplyr)
library(ggplot2)
library(tidyr)

ss <- readRDS("cluster/ss.bounds.rds")

ss.plot <-
  ss%>%
  dplyr::mutate(flabel = sprintf('M2:%s', M2))%>%
  ggplot(aes(x=p_C, y = n.arm, fill = method)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  theme_bw() +
  facet_wrap(~flabel, labeller = ggplot2::label_parsed) +
  theme(legend.position = "bottom") +
  scale_fill_discrete(labels=c('FM','Wald','NW')) + 
  labs(x = "Event probability in control group",
       y = "N per arm",
       fill = "Method") +
  theme(text=element_text(size=15))



pdf("cluster/checks/ss_per_method.pdf")
ss.plot
dev.off()

