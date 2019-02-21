library(ggplot2)
library(dplyr)

cp.025.wald <- readRDS("True_CP/summary/bineval.025.cp.wald.rds")
cp.10.wald <- readRDS("True_CP/summary/bineval.10.cp.wald.rds")
cp.20.wald <- readRDS("True_CP/summary/bineval.20.cp.wald.rds")

cp.025.wilson <- readRDS("True_CP/summary/bineval.025.cp.wilson.rds")
cp.10.wilson <- readRDS("True_CP/summary/bineval.10.cp.wilson.rds")
cp.20.wilson <- readRDS("True_CP/summary/bineval.20.cp.wilson.rds")

cp.025.fm <- readRDS("True_CP/summary/bineval.025.cp.fm.rds")
cp.10.fm <- readRDS("True_CP/summary/bineval.10.cp.fm.rds")
cp.20.fm <- readRDS("True_CP/summary/bineval.20.cp.fm.rds")

cp.025 <- bind_rows(cp.025.wald%>%
                      mutate(Method='Wald'),
                    cp.025.wilson%>%
                      mutate(Method='Newcombe'),
                    cp.025.fm%>%
                      mutate(Method='Farrington-Manning'))%>%
  mutate(Delta=0.025)

cp.10 <- bind_rows(cp.10.wald%>%
                      mutate(Method='Wald'),
                    cp.10.wilson%>%
                      mutate(Method='Newcombe'),
                    cp.10.fm%>%
                     mutate(Method='Farrington-Manning'))%>%
  mutate(Delta=0.10)

cp.20 <- bind_rows(cp.20.wald%>%
                     mutate(Method='Wald'),
                   cp.20.wilson%>%
                     mutate(Method='Newcombe'),
                   cp.20.fm%>%
                     mutate(Method='Farrington-Manning'))%>%
  mutate(Delta=0.20)

cp.all <- bind_rows(cp.025, cp.10, cp.20)%>%
  filter(n1<=2100)%>%
  mutate(Delta = sprintf("Delta ~ %g",Delta),
         p1 = sprintf("p[C] ~ %g",p1))

cp.all.plot <- cp.all%>%
  ggplot2::ggplot(aes(x = n1, y = cp, group=Method)) +
  ggplot2::geom_line(aes(color=Method)) +
  ggplot2::geom_point(size=0.1, aes(color=Method)) +
  ggplot2::geom_hline(yintercept=0.95, colour="red") +
  ggplot2::geom_vline(xintercept=90, colour="grey") +
  ggplot2::scale_y_continuous(limits = c(0.94, 0.97)) +
  theme(legend.position = "bottom",
        text = element_text(size = 13),
        strip.text = element_text(size = rel(0.7)))+
  xlab("N per treatment group") +
  ylab("True coverage probability") +
  scale_x_continuous(limits = c(90,2100), breaks = c(90,seq(500,1500,500),2100)) +
  facet_wrap(Delta~p1, labeller = label_parsed)


pdf("True_CP/Graphs/CP_all.pdf")
cp.all.plot
dev.off()


###################
### SMALL SS #####
##################

cp.025.wald.small <- readRDS("True_CP/summary/bineval.025.cp.wald.small.rds")
cp.10.wald.small <- readRDS("True_CP/summary/bineval.10.cp.wald.small.rds")
cp.20.wald.small <- readRDS("True_CP/summary/bineval.20.cp.wald.small.rds")

cp.025.wilson.small <- readRDS("True_CP/summary/bineval.025.cp.wilson.small.rds")
cp.10.wilson.small <- readRDS("True_CP/summary/bineval.10.cp.wilson.small.rds")
cp.20.wilson.small <- readRDS("True_CP/summary/bineval.20.cp.wilson.small.rds")

cp.025.fm.small <- readRDS("True_CP/summary/bineval.025.cp.fm.small.rds")
cp.10.fm.small <- readRDS("True_CP/summary/bineval.10.cp.fm.small.rds")
cp.20.fm.small <- readRDS("True_CP/summary/bineval.20.cp.fm.small.rds")

cp.025.small <- bind_rows(cp.025.wald.small%>%
                      mutate(Method='Wald'),
                    cp.025.wilson.small%>%
                      mutate(Method='Newcombe'),
                    cp.025.fm.small%>%
                      mutate(Method='Farrington-Manning'))%>%
  mutate(Delta=0.025)

cp.10.small <- bind_rows(cp.10.wald.small%>%
                     mutate(Method='Wald'),
                   cp.10.wilson.small%>%
                     mutate(Method='Newcombe'),
                     cp.10.fm.small%>%
                     mutate(Method='Farrington-Manning'))%>%
  mutate(Delta=0.10)

cp.20.small <- bind_rows(cp.20.wald.small%>%
                     mutate(Method='Wald'),
                   cp.20.wilson.small%>%
                     mutate(Method='Newcombe'),
                   cp.20.fm.small%>%
                     mutate(Method='Farrington-Manning'))%>%
  mutate(Delta=0.20)

cp.all.small <- bind_rows(cp.025.small, cp.10.small, cp.20.small)%>%
  mutate(Delta = sprintf("Delta ~ %g",Delta),
         p1 = sprintf("p_C ~ %g",p1))

cp.all.plot.small <- cp.all.small%>%
  filter(n1>=20)%>%
  ggplot2::ggplot(aes(x = n1, y = cp, group=Method)) +
  ggplot2::geom_line(aes(color=Method)) +
  ggplot2::geom_point(size=0.1, aes(color=Method)) +
  ggplot2::geom_hline(yintercept=0.95, colour="red") +
  #ggplot2::scale_y_continuous(limits = seq(0.9,1,0.1)) +
  theme(legend.position = "bottom",
        text = element_text(size = 13),
        strip.text = element_text(size = rel(0.7)))+
  xlab("N per treatment group") +
  ylab("True coverage probability") +
  scale_x_continuous(breaks = seq(20,90,10)) +
  facet_wrap(Delta~p1, labeller = label_parsed)


pdf("True_CP/Graphs/CP_all_small.pdf")
cp.all.plot.small
dev.off()
