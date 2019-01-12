source("Step_0_init.R")

#####################################################
####  Type I Error Wald, FM and WN initial dataset ##
#####################################################

dt.full <- readRDS('DataSummaries/dtfullsum_each.rds')

dt.full.wald <- dt.full%>%
  dplyr::select(p_C, M2, type1.Wald)%>%
  reshape2::melt(id.vars = c("p_C","M2"))%>%
  dplyr::rename(type1full = value)%>%
  dplyr::mutate(TYPE='wald')%>%
  dplyr::select(-variable)

dt.full.fm <- dt.full%>%
  dplyr::select(p_C, M2, type1.FM)%>%
  reshape2::melt(id.vars = c("p_C","M2"))%>%
  dplyr::rename(type1full = value)%>%
  dplyr::mutate(TYPE='fm')%>%
  dplyr::select(-variable)

dt.full.wn <- dt.full%>%
  dplyr::select(p_C, M2, type1.WN)%>%
  reshape2::melt(id.vars = c("p_C","M2"))%>%
  dplyr::rename(type1full = value)%>%
  dplyr::mutate(TYPE='wn')%>%
  dplyr::select(-variable)


dt.full1 <- dt.full.wald%>%
  dplyr::bind_rows(dt.full.fm, dt.full.wn)%>%
  dplyr::right_join(dt.full%>%
                      dplyr::select(scenario.id, p_C, M2), by=c('p_C','M2'))

cca <- readRDS('DataSummaries/cca.rds')

cca.full <- cca%>%
  dplyr::rename(p_C=p_T)%>%
  dplyr::left_join(dt.full1%>%
                     dplyr::select(scenario.id, TYPE, type1full), by = c('scenario.id','TYPE'))

#CCA Wald MCAR
type1.g <- type1.cca.plot(df = cca.full, type = 'wald', missing = 'mcar', title = 'Wald, MCAR')
pdf("final_outputs/type1_ccawaldmcar.pdf")
type1.g
dev.off()

#CCA Wald MAR
type1.g <- type1.cca.plot(df = cca.full, type = 'wald', missing = 'mar', title = 'Wald, MAR')
pdf("final_outputs/type1_ccawaldmar.pdf")
type1.g
dev.off()

#CCA Wald MNAR
type1.g <- type1.cca.plot(df = cca.full, type = 'wald', missing = 'mnar', title = 'Wald, MNAR')
pdf("final_outputs/type1_ccawaldmnar.pdf")
type1.g
dev.off()

#CCA FM MCAR
type1.g <- type1.cca.plot(df = cca.full, type = 'fm', missing = 'mcar', title = 'FM, MCAR')
pdf("final_outputs/type1_ccafmmcar.pdf")
type1.g
dev.off()

#CCA FM MAR
type1.g <- type1.cca.plot(df = cca.full, type = 'fm', missing = 'mar', title = 'FM, MAR')
pdf("final_outputs/type1_ccafmmar.pdf")
type1.g
dev.off()

#CCA FM MNAR
type1.g <- type1.cca.plot(df = cca.full, type = 'fm', missing = 'mnar', title = 'FM, MNAR')
pdf("final_outputs/type1_ccafmmnar.pdf")
type1.g
dev.off()

#CCA WN MCAR
type1.g <- type1.cca.plot(df = cca.full, type = 'wn', missing = 'mcar', title = 'WN, MCAR')
pdf("final_outputs/type1_ccawnmcar.pdf")
type1.g
dev.off()

#CCA WN MAR
type1.g <- type1.cca.plot(df = cca.full, type = 'wn', missing = 'mar', title = 'WN, MAR')
pdf("final_outputs/type1_ccawnmar.pdf")
type1.g
dev.off()


#CCA FM MNAR
type1.g <- type1.cca.plot(df = cca.full, type = 'wn', missing = 'mnar', title = 'WN, MNAR')
pdf("final_outputs/type1_ccawnmnar.pdf")
type1.g
dev.off()

#####################################################
####  Type I Error Wald b.X=-log(10)               ##
#####################################################


#add new graphs after chaging the missingness model parameters: b.X=-log(10)

ccawaldnew <- readRDS('DataSummaries/ccawaldnew.rds')

ccawaldnew.full <- ccawaldnew%>%
  dplyr::rename(p_C=p_T)%>%
  dplyr::left_join(dt.full1%>%
                     dplyr::select(scenario.id, TYPE, type1full), by = c('scenario.id','TYPE'))


type1.g <- type1.cca.plot(df = ccawaldnew.full, type = 'wald', missing = 'mar', title = 'Wald, MAR New')
pdf("final_outputs/type1_ccawaldmarnew.pdf")
type1.g
dev.off()

type1.g <- type1.cca.plot(df = ccawaldnew.full, type = 'wald', missing = 'mnar', title = 'Wald, MNAR New',
                          ylim = c(0, 0.35))
pdf("final_outputs/type1_ccawaldmnarnew.pdf")
type1.g
dev.off()


######################################################
####  Relative Bias Wald, FM and WN initial dataset ##
######################################################
#relative bias defined as: bias = (C_phat-T_phat-M2)/M2

cca <- readRDS('DataSummaries/cca.rds')

# Relative bias Wald MCAR
bias.g <- bias.cca.plot(df = cca, type = 'wald', missing = 'mcar', title = 'Wald, MCAR')
pdf("final_outputs/bias_ccawaldmcar.pdf")
bias.g
dev.off()

# Relative bias Wald MAR
bias.g <- bias.cca.plot(df = cca, type = 'wald', missing = 'mar', title = 'Wald, MAR',
                        ylim=c(-0.05,0.05))
pdf("final_outputs/bias_ccawaldmar.pdf")
bias.g
dev.off()

# Relative bias Wald MNAR
bias.g <- bias.cca.plot(df = cca, type = 'wald', missing = 'mnar', title = 'Wald, MNAR',
                        ylim=c(-0.05,0.3))
pdf("final_outputs/bias_ccawaldmnar.pdf")
bias.g
dev.off()

# Relative bias FM MCAR
bias.g <- bias.cca.plot(df = cca, type = 'fm', missing = 'mcar', title = 'FM, MCAR')
pdf("final_outputs/bias_ccafmmcar.pdf")
bias.g
dev.off()

# Relative bias FM MAR
bias.g <- bias.cca.plot(df = cca, type = 'fm', missing = 'mar', title = 'FM, MAR',
                        ylim=c(-0.05,0.05))
pdf("final_outputs/bias_ccafmmar.pdf")
bias.g
dev.off()

# Relative bias FM MNAR
bias.g <- bias.cca.plot(df = cca, type = 'fm', missing = 'mnar', title = 'FM, MNAR',
                        ylim=c(-0.05,0.3))
pdf("final_outputs/bias_ccafmmnar.pdf")
bias.g
dev.off()

# Relative bias WN MCAR
bias.g <- bias.cca.plot(df = cca, type = 'wn', missing = 'mcar', title = 'WN, MCAR')
pdf("final_outputs/bias_ccawnmcar.pdf")
bias.g
dev.off()

# Relative bias WN MAR
bias.g <- bias.cca.plot(df = cca, type = 'wn', missing = 'mar', title = 'WN, MAR',
                        ylim=c(-0.05,0.05))
pdf("final_outputs/bias_ccawnmar.pdf")
bias.g
dev.off()

# Relative bias WN MNAR
bias.g <- bias.cca.plot(df = cca, type = 'wn', missing = 'mnar', title = 'WN, MNAR',
                        ylim=c(-0.05,0.3))
pdf("final_outputs/bias_ccawnmnar.pdf")
bias.g
dev.off()

######################################################
####  Relative Bias Wald, FM and WN b.X=-log(10)    ##
######################################################
#relative bias defined as: bias = (C_phat-T_phat-M2)/M2

ccawaldnew <- readRDS('DataSummaries/ccawaldnew.rds')

# Relative bias Wald MAR
bias.g <- bias.cca.plot(df = ccawaldnew, type = 'wald', missing = 'mar', title = 'Wald, MAR New',
                        ylim=c(-0.05,0.05))
pdf("final_outputs/bias_ccawaldmarnew.pdf")
bias.g
dev.off()

# Relative bias Wald MNAR
bias.g <- bias.cca.plot(df = ccawaldnew, type = 'wald', missing = 'mnar', title = 'Wald, MNAR New',
                        ylim=c(-0.5,0.01))
pdf("final_outputs/bias_ccawaldmnarnew.pdf")
bias.g
dev.off()



#####################################################
#### CCA Analysis Graphs - Power        #############
#####################################################

dt.full <- readRDS('DataSummaries/dtfullsum_each.rds')

dt.full.wald <- dt.full%>%
  dplyr::select(p_C, M2, power.Wald)%>%
  reshape2::melt(id.vars = c("p_C","M2"))%>%
  dplyr::rename(powerfull = value)%>%
  dplyr::mutate(TYPE='wald')%>%
  dplyr::select(-variable)

dt.full.fm <- dt.full%>%
  dplyr::select(p_C, M2, power.FM)%>%
  reshape2::melt(id.vars = c("p_C","M2"))%>%
  dplyr::rename(powerfull = value)%>%
  dplyr::mutate(TYPE='fm')%>%
  dplyr::select(-variable)

dt.full.wn <- dt.full%>%
  dplyr::select(p_C, M2, power.WN)%>%
  reshape2::melt(id.vars = c("p_C","M2"))%>%
  dplyr::rename(powerfull = value)%>%
  dplyr::mutate(TYPE='wn')%>%
  dplyr::select(-variable)

dt.full1 <- dt.full.wald%>%
  dplyr::bind_rows(dt.full.fm, dt.full.wn)%>%
  dplyr::right_join(dt.full%>%
                      dplyr::select(scenario.id, p_C, M2), by=c('p_C','M2'))

cca <- readRDS('DataSummaries/cca.rds')

cca.full <- cca%>%
  dplyr::rename(p_C=p_T)%>%
  dplyr::right_join(dt.full1%>%
                      dplyr::select(scenario.id, TYPE, powerfull), by = c('scenario.id','TYPE'))

#CCA Wald MCAR
power.gdt <- cca.full%>%
  dplyr::filter(TYPE=='wald', MISSING=='mcar')%>%
  dplyr::mutate(scenario.idc = sprintf('p=%s & M=%s',p_C,M2))


power.g <- power.gdt%>%
  ggplot2::ggplot(aes(PERCENT, power)) +
  geom_segment(aes(x = PERCENT, y = powerfull, xend = PERCENT, yend = power), colour="red") +
  geom_point(size=0.5) +
  scale_y_continuous(limits = c(0.65,1)) +
  facet_wrap(~ scenario.idc) +
  theme_bw() +
  xlab("% of Missing Observations")+
  ylab("Power")+
  ggtitle("Power: Wald, MCAR, CCA Analysis")


pdf("final_outputs/power_ccawaldmcar.pdf")
power.g
dev.off()

#CCA Wald MAR
power.gdt <- cca.full%>%
  dplyr::filter(TYPE=='wald', MISSING=='mar')%>%
  dplyr::mutate(scenario.idc = sprintf('p=%s & M=%s',p_C,M2))


power.g <- power.gdt%>%
  ggplot2::ggplot(aes(PERCENT, power)) +
  geom_segment(aes(x = PERCENT, y = powerfull, xend = PERCENT, yend = power), colour="red") +
  geom_point(size=0.5) +
  scale_y_continuous(limits = c(0.65,1)) +
  facet_wrap(~ scenario.idc) +
  theme_bw() +
  xlab("% of Missing Observations")+
  ylab("Power")+
  ggtitle("Power: Wald, MAR, CCA Analysis")


pdf("final_outputs/power_ccawaldmar.pdf")
power.g
dev.off()


#CCA Wald MNAR
power.gdt <- cca.full%>%
  dplyr::filter(TYPE=='wald', MISSING=='mnar')%>%
  dplyr::mutate(scenario.idc = sprintf('p=%s & M=%s',p_C,M2))


power.g <- power.gdt%>%
  ggplot2::ggplot(aes(PERCENT, power)) +
  geom_segment(aes(x = PERCENT, y = powerfull, xend = PERCENT, yend = power), colour="red") +
  geom_point(size=0.5) +
  scale_y_continuous(limits = c(0.65,1)) +
  facet_wrap(~ scenario.idc) +
  theme_bw() +
  xlab("% of Missing Observations")+
  ylab("Power")+
  ggtitle("Power: Wald, MNAR, CCA Analysis")


pdf("final_outputs/power_ccawaldmnar.pdf")
power.g
dev.off()

##### Stopped here
#CCA FM MCAR
type1.gdt <- cca.full%>%
  dplyr::filter(TYPE=='fm', MISSING=='mcar')%>%
  dplyr::mutate(scenario.idc = sprintf('p=%s & M=%s',p_C,M2))


type1.g <- type1.gdt%>%
  ggplot2::ggplot(aes(PERCENT, type1)) +
  geom_segment(aes(x = PERCENT, y = type1full, xend = PERCENT, yend = type1), colour="red") +
  geom_point(size=0.5) +
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2) +
  scale_y_continuous(limits = c(0,0.05)) +
  facet_wrap(~ scenario.idc) +
  theme_bw() +
  xlab("% of Missing Observations")+
  ylab("Type-I error")+
  ggtitle("Type I Error: FM, MCAR, CCA Analysis")


pdf("final_outputs/type1_ccafmmcar.pdf")
type1.g
dev.off()

#CCA FM MAR
type1.gdt <- cca.full%>%
  dplyr::filter(TYPE=='fm', MISSING=='mar')%>%
  dplyr::mutate(scenario.idc = sprintf('p=%s & M=%s',p_C,M2))


type1.g <- type1.gdt%>%
  ggplot2::ggplot(aes(PERCENT, type1)) +
  geom_segment(aes(x = PERCENT, y = type1full, xend = PERCENT, yend = type1), colour="red") +
  geom_point(size=0.5) +
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2) +
  scale_y_continuous(limits = c(0,0.05)) +
  facet_wrap(~ scenario.idc) +
  theme_bw() +
  xlab("% of Missing Observations")+
  ylab("Type-I error")+
  ggtitle("Type I Error: FM, MAR, CCA Analysis")


pdf("final_outputs/type1_ccafmmar.pdf")
type1.g
dev.off()


#CCA FM MNAR
type1.gdt <- cca.full%>%
  dplyr::filter(TYPE=='fm', MISSING=='mnar')%>%
  dplyr::mutate(scenario.idc = sprintf('p=%s & M=%s',p_C,M2))


type1.g <- type1.gdt%>%
  ggplot2::ggplot(aes(PERCENT, type1)) +
  geom_segment(aes(x = PERCENT, y = type1full, xend = PERCENT, yend = type1), colour="red") +
  geom_point(size=0.5) +
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2) +
  scale_y_continuous(limits = c(0,0.05)) +
  facet_wrap(~ scenario.idc) +
  theme_bw() +
  xlab("% of Missing Observations")+
  ylab("Type-I error")+
  ggtitle("Type I Error: FM, MNAR, CCA Analysis")


pdf("final_outputs/type1_ccafmmnar.pdf")
type1.g
dev.off()


#CCA WN MCAR
type1.gdt <- cca.full%>%
  dplyr::filter(TYPE=='wn', MISSING=='mcar')%>%
  dplyr::mutate(scenario.idc = sprintf('p=%s & M=%s',p_C,M2))


type1.g <- type1.gdt%>%
  ggplot2::ggplot(aes(PERCENT, type1)) +
  geom_segment(aes(x = PERCENT, y = type1full, xend = PERCENT, yend = type1), colour="red") +
  geom_point(size=0.5) +
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2) +
  scale_y_continuous(limits = c(0,0.05)) +
  facet_wrap(~ scenario.idc) +
  theme_bw() +
  xlab("% of Missing Observations")+
  ylab("Type-I error")+
  ggtitle("Type I Error: WN, MCAR, CCA Analysis")


pdf("final_outputs/type1_ccawnmcar.pdf")
type1.g
dev.off()

#CCA WN MAR
type1.gdt <- cca.full%>%
  dplyr::filter(TYPE=='wn', MISSING=='mar')%>%
  dplyr::mutate(scenario.idc = sprintf('p=%s & M=%s',p_C,M2))


type1.g <- type1.gdt%>%
  ggplot2::ggplot(aes(PERCENT, type1)) +
  geom_segment(aes(x = PERCENT, y = type1full, xend = PERCENT, yend = type1), colour="red") +
  geom_point(size=0.5) +
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2) +
  scale_y_continuous(limits = c(0,0.05)) +
  facet_wrap(~ scenario.idc) +
  theme_bw() +
  xlab("% of Missing Observations")+
  ylab("Type-I error")+
  ggtitle("Type I Error: WN, MAR, CCA Analysis")


pdf("final_outputs/type1_ccawnmar.pdf")
type1.g
dev.off()


#CCA FM MNAR
type1.gdt <- cca.full%>%
  dplyr::filter(TYPE=='wn', MISSING=='mnar')%>%
  dplyr::mutate(scenario.idc = sprintf('p=%s & M=%s',p_C,M2))


type1.g <- type1.gdt%>%
  ggplot2::ggplot(aes(PERCENT, type1)) +
  geom_segment(aes(x = PERCENT, y = type1full, xend = PERCENT, yend = type1), colour="red") +
  geom_point(size=0.5) +
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2) +
  scale_y_continuous(limits = c(0,0.05)) +
  facet_wrap(~ scenario.idc) +
  theme_bw() +
  xlab("% of Missing Observations")+
  ylab("Type-I error")+
  ggtitle("Type I Error: WN, MNAR, CCA Analysis")


pdf("final_outputs/type1_ccawnmnar.pdf")
type1.g
dev.off()



