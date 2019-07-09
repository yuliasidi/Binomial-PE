library(ggplot2)
library(dplyr)


###################################
####### Plot CP ###################
###################################

cp.025.wald <- readRDS("True_CP/summary/bineval.025.cp.wald.rds")
cp.10.wald <- readRDS("True_CP/summary/bineval.10.cp.wald.rds")
cp.20.wald <- readRDS("True_CP/summary/bineval.20.cp.wald.rds")

cp.025.wilson <- readRDS("True_CP/summary/bineval.025.cp.wilson.rds")
cp.10.wilson <- readRDS("True_CP/summary/bineval.10.cp.wilson.rds")
cp.20.wilson <- readRDS("True_CP/summary/bineval.20.cp.wilson.rds")

cp.025.fm <- readRDS("True_CP/summary/bineval.025.fm.all.rds")
cp.10.fm <- readRDS("True_CP/summary/bineval.10.fm.all.rds")
cp.20.fm <- readRDS("True_CP/summary/bineval.20.fm.all.rds")

cp.025 <- bind_rows(cp.025.wald%>%
                      mutate(Method='wald'),
                    cp.025.wilson%>%
                      mutate(Method='wn'),
                    cp.025.fm%>%
                      dplyr::select(-el,-na.sum)%>%
                      mutate(Method='fm'))%>%
  mutate(Delta=0.025)

cp.10 <- bind_rows(cp.10.wald%>%
                      mutate(Method='wald'),
                    cp.10.wilson%>%
                      mutate(Method='wn'),
                    cp.10.fm%>%
                     dplyr::select(-el,-na.sum)%>%
                     mutate(Method='fm'))%>%
  mutate(Delta=0.10)

cp.20 <- bind_rows(cp.20.wald%>%
                     mutate(Method='wald'),
                   cp.20.wilson%>%
                     mutate(Method='wn'),
                   cp.20.fm%>%
                     dplyr::select(-el,-na.sum)%>%
                     mutate(Method='fm'))%>%
  mutate(Delta=0.20)

cp.all <- bind_rows(cp.025, cp.10, cp.20)%>%
  filter(n1<=2000)%>%
  mutate(flabel = sprintf('Delta: %s ~ p[C]: %s',Delta, p1))

cp.all.plot <- 
  cp.all%>%
  ggplot2::ggplot(aes(x = n1, y = cp, group=Method)) +
  ggplot2::geom_line(aes(color=Method)) +
  ggplot2::geom_point(size=0.1, aes(color=Method)) +
  ggplot2::geom_hline(yintercept=0.95, colour="black") +
  ggplot2::scale_y_continuous(limits = c(0.94,0.965), breaks = c(0.94, 0.95, 0.96)) +
  xlab("N per treatment group") +
  ylab("True coverage probability") +
  scale_x_continuous(limits = c(90,2000), breaks = c(100, seq(500,2000,500))) +
  scale_color_discrete(labels=c('FM','Wald','WN')) +
  facet_wrap(~flabel, ncol = 4, nrow = 3,
             labeller = label_parsed) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 13),
        strip.text = element_text(size = rel(0.7)))


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

cp.025.fm.small <- readRDS("True_CP/summary/bineval.025.fm.all.sm.rds")
cp.10.fm.small <- readRDS("True_CP/summary/bineval.10.fm.all.sm.rds")
cp.20.fm.small <- readRDS("True_CP/summary/bineval.20.fm.all.sm.rds")

cp.025.small <- bind_rows(cp.025.wald.small%>%
                      mutate(Method='wald'),
                    cp.025.wilson.small%>%
                      mutate(Method='wn'),
                    cp.025.fm.small%>%
                      dplyr::select(-el,-na.sum)%>%
                      mutate(Method='fm'))%>%
  mutate(Delta=0.025)

cp.10.small <- bind_rows(cp.10.wald.small%>%
                     mutate(Method='wald'),
                   cp.10.wilson.small%>%
                     mutate(Method='wn'),
                     cp.10.fm.small%>%
                     dplyr::select(-el,-na.sum)%>%
                     mutate(Method='fm'))%>%
  mutate(Delta=0.10)

cp.20.small <- bind_rows(cp.20.wald.small%>%
                     mutate(Method='wald'),
                   cp.20.wilson.small%>%
                     mutate(Method='wn'),
                   cp.20.fm.small%>%
                     dplyr::select(-el,-na.sum)%>%
                     mutate(Method='fm'))%>%
  mutate(Delta=0.20)

cp.all.small <- bind_rows(cp.025.small, cp.10.small, cp.20.small)%>%
  mutate(flabel = sprintf('Delta: %s ~ p[C]: %s',Delta, p1))

cp.all.plot.small <- 
  cp.all.small%>%
  filter(n1>=20)%>%
  ggplot2::ggplot(aes(x = n1, y = cp, group=Method)) +
  ggplot2::geom_line(aes(color=Method)) +
  ggplot2::geom_point(size=0.1, aes(color=Method)) +
  ggplot2::geom_hline(yintercept=0.95, colour="black") +
  ggplot2::scale_y_continuous(limits = c(0.90,1), breaks = c(0.90, 0.925, 0.95, 0.975, 1)) +
  scale_x_continuous(breaks = seq(20,90,10)) +
  scale_color_discrete(labels=c('FM','Wald','WN')) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 13),
        strip.text = element_text(size = rel(0.7)))+
  xlab("N per treatment group") +
  ylab("True coverage probability") +
  facet_wrap(~flabel, ncol = 4, nrow = 3, labeller = label_parsed)


  pdf("True_CP/Graphs/CP_all_small.pdf")
  cp.all.plot.small
  dev.off()

###################################
####### Plot EL ###################
###################################


el.025.wald <- readRDS("True_CP/summary/Expected_length/bineval.025.el.wald.rds")
el.10.wald <- readRDS("True_CP/summary/Expected_length/bineval.10.el.wald.rds")
el.20.wald <- readRDS("True_CP/summary//Expected_length/bineval.20.el.wald.rds")

el.025.wilson <- readRDS("True_CP/summary/Expected_length/bineval.025.el.wilson.rds")
el.10.wilson <- readRDS("True_CP/summary/Expected_length/bineval.10.el.wilson.rds")
el.20.wilson <- readRDS("True_CP/summary/Expected_length/bineval.20.el.wilson.rds")

el.025.fm <- readRDS("True_CP/summary/bineval.025.fm.all.rds")
el.10.fm <- readRDS("True_CP/summary/bineval.10.fm.all.rds")
el.20.fm <- readRDS("True_CP/summary/bineval.20.fm.all.rds")

el.025 <- bind_rows(el.025.wald%>%
                      mutate(Method='wald'),
                    el.025.wilson%>%
                      mutate(Method='wn'),
                    el.025.fm%>%
                      dplyr::select(-cp,-na.sum)%>%
                      mutate(Method='fm'))%>%
  mutate(Delta=0.025)

el.10 <- bind_rows(el.10.wald%>%
                     mutate(Method='wald'),
                   el.10.wilson%>%
                     mutate(Method='wn'),
                   el.10.fm%>%
                     dplyr::select(-cp,-na.sum)%>%
                     mutate(Method='fm'))%>%
  mutate(Delta=0.10)

el.20 <- bind_rows(el.20.wald%>%
                     mutate(Method='wald'),
                   el.20.wilson%>%
                     mutate(Method='wn'),
                   el.20.fm%>%
                     dplyr::select(-cp,-na.sum)%>%
                     mutate(Method='fm'))%>%
  mutate(Delta=0.20)

el.all <- bind_rows(el.025, el.10, el.20)%>%
  filter(n1<=2000)%>%
  mutate(flabel = sprintf('Delta: %s ~ p[C]: %s',Delta, p1))

el.all.plot <- 
el.all%>%
  ggplot2::ggplot(aes(x = n1, y = el, group=Method)) +
  ggplot2::geom_line(aes(color=Method)) +
  ggplot2::geom_point(size=0.1, aes(color=Method)) +
  #ggplot2::geom_hline(yintercept=0.95, colour="black") +
  #ggplot2::geom_vline(xintercept=90, colour="grey") +
  ggplot2::scale_y_continuous(limits = c(0.025, 0.3)) +
  scale_color_discrete(labels=c('FM','Wald','WN')) +
  xlab("N per treatment group") +
  ylab("CI Expected Length") +
  scale_x_continuous(limits = c(90,2000), breaks = c(100, seq(500,2000,500))) +
  facet_wrap(~flabel, ncol = 4, nrow = 3,
             labeller = label_parsed) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 13),
        strip.text = element_text(size = rel(0.7)))

pdf("True_CP/Graphs/EL_all.pdf")
el.all.plot
dev.off()


###################
### SMALL SS #####
##################

el.025.wald.small <- readRDS("True_CP/summary/Expected_length/bineval.025.el.wald.sm.rds")
el.10.wald.small <- readRDS("True_CP/summary/Expected_length/bineval.10.el.wald.sm.rds")
el.20.wald.small <- readRDS("True_CP/summary/Expected_length/bineval.20.el.wald.sm.rds")

el.025.wilson.small <- readRDS("True_CP/summary/Expected_length/bineval.025.el.wilson.sm.rds")
el.10.wilson.small <- readRDS("True_CP/summary/Expected_length/bineval.10.el.wilson.sm.rds")
el.20.wilson.small <- readRDS("True_CP/summary/Expected_length/bineval.20.el.wilson.sm.rds")

el.025.fm.small <- readRDS("True_CP/summary/bineval.025.fm.all.sm.rds")
el.10.fm.small <- readRDS("True_CP/summary/bineval.10.fm.all.sm.rds")
el.20.fm.small <- readRDS("True_CP/summary/bineval.20.fm.all.sm.rds")

el.025.small <- bind_rows(el.025.wald.small%>%
                            mutate(Method='wald'),
                          el.025.wilson.small%>%
                            mutate(Method='wn'),
                          el.025.fm.small%>%
                            dplyr::select(-cp,-na.sum)%>%
                            mutate(Method='fm'))%>%
  mutate(Delta=0.025)

el.10.small <- bind_rows(el.10.wald.small%>%
                           mutate(Method='wald'),
                         el.10.wilson.small%>%
                           mutate(Method='wn'),
                         el.10.fm.small%>%
                           dplyr::select(-cp,-na.sum)%>%
                           mutate(Method='fm'))%>%
  mutate(Delta=0.10)

el.20.small <- bind_rows(el.20.wald.small%>%
                           mutate(Method='wald'),
                         el.20.wilson.small%>%
                           mutate(Method='wn'),
                         el.20.fm.small%>%
                           dplyr::select(-cp,-na.sum)%>%
                           mutate(Method='fm'))%>%
  mutate(Delta=0.20)

el.all.small <- bind_rows(el.025.small, el.10.small, el.20.small)%>%
  mutate(flabel = sprintf('Delta: %s ~ p[C]: %s',Delta, p1))

el.all.plot.small <- 
el.all.small%>%
  filter(n1>=20)%>%
  ggplot2::ggplot(aes(x = n1, y = el, group=Method)) +
  ggplot2::geom_line(aes(color=Method)) +
  ggplot2::geom_point(size=0.1, aes(color=Method)) +
  scale_color_discrete(labels=c('FM','Wald','WN')) +
  #ggplot2::geom_hline(yintercept=0.95, colour="black") +
  #ggplot2::scale_y_continuous(limits = seq(0.9,1,0.1)) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 13),
        strip.text = element_text(size = rel(0.7)))+
  xlab("N per treatment group") +
  ylab("CI Expected Length") +
  scale_x_continuous(breaks = seq(20,90,10)) +
  facet_wrap(~flabel, ncol = 4, nrow = 3,
             labeller = label_parsed)
  

pdf("True_CP/Graphs/EL_all_small.pdf")
el.all.plot.small
dev.off()