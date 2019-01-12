source("Step_0_init.R")


##############################
### Sample Size Comparison ###
##############################

SS <- readRDS("SS_Wald_FM_WN_GLM_power90.rds")

SS.plot <- SS%>%select(scenario.id, N.total.Wald, N.total.FM, N.total.WN, N.total.GLM)%>%
  melt(id.vars = "scenario.id")%>%
  mutate(Method = case_when(variable == "N.total.Wald" ~ "Wald",
                            variable == "N.total.FM"   ~ "Farrington-Manning",
                            variable == "N.total.WN"   ~ "Wilson-Newcombe",
                            variable == "N.total.GLM"  ~ "GLM"))%>%
  select(-variable)

# ggplot(SS.plot, aes(fill = Method, y = value, x = scenario.id)) +
#   geom_bar(position = "dodge", stat = "identity") +
#   theme_bw()+
#   theme(legend.position = "bottom")+
#   xlab("Scenario ID")+
#   ylab("Total sample size")+
#   scale_x_continuous(labels = as.character(scenario.id), breaks = as.numeric(scenario.id))

SS.plot.out <- SS.plot%>%
  ggplot() +
  geom_point(aes(colour = Method, x = value, y = scenario.id)) +
  geom_segment(aes(y=scenario.id,
                   yend=scenario.id,
                   x=min,
                   xend=max),
               linetype=2,
               alpha=.3,
                  data=SS.plot%>%
                    group_by(scenario.id)%>%
                    summarise_at(vars(value),funs(min,max))) +
  theme_bw() +
  scale_y_continuous(breaks=1:30)+
  theme(legend.position = "bottom")+
  ylab("Scenario ID")+
  xlab("Total sample size")

SS.plotly.out <- plotly::ggplotly(SS.plot.out)

htmlwidgets::saveWidget(SS.plotly.out,"SS_plot.html")  
  

###################################
### Type-I Error - NMax vs Nmin ###
###################################

dt.nmax <- readRDS('dtfullsum_nmax10000.rds')
dt.nmin <- readRDS('dtfullsum_nmin10000.rds')

dt.maxmin <- dt.nmax%>%
  dplyr::mutate(ss = 'Nmax')%>%
  dplyr::bind_rows(dt.nmin%>%
                     dplyr::mutate(ss = 'Nmin'))

type1.wald.plot <- dt.maxmin%>%
  ggplot(aes(x=p_C, y=type1.Wald, group=ss))+
  geom_point(aes(color=ss))+
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2)+
  theme_bw()+
  #scale_x_continuous(labels = as.character(p_C), breaks = p_C)+
  scale_y_continuous(limits = c(0,0.05))+
  xlab("Proportion of events in active treatment")+
  ylab("Type-I error")+
  theme(legend.position = "bottom")+
  ggtitle("Type-I errors for 10000 simulations for Wald")+
  facet_wrap(~M2)

type1.fm.plot <- dt.maxmin%>%
  ggplot(aes(x=p_C, y=type1.FM, group=ss))+
  geom_point(aes(color=ss))+
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2)+
  theme_bw()+
  #scale_x_continuous(labels = as.character(p_C), breaks = p_C)+
  scale_y_continuous(limits = c(0,0.05))+
  xlab("Proportion of events in active treatment")+
  ylab("Type-I error")+
  theme(legend.position = "bottom")+
  ggtitle("Type-I errors for 10000 simulations for FM")+
  facet_wrap(~M2)

type1.wn.plot <- dt.maxmin%>%
  ggplot(aes(x=p_C, y=type1.WN, group=ss))+
  geom_point(aes(color=ss))+
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2)+
  theme_bw()+
  #scale_x_continuous(labels = as.character(p_C), breaks = p_C)+
  scale_y_continuous(limits = c(0,0.05))+
  xlab("Proportion of events in active treatment")+
  ylab("Type-I error")+
  theme(legend.position = "bottom")+
  ggtitle("Type-I errors for 10000 simulations for WN")+
  facet_wrap(~M2)


pdf("minmax_type1_wald.pdf")
type1.wald.plot
dev.off()

pdf("minmax_type1_fm.pdf")
type1.fm.plot
dev.off()

pdf("minmax_type1_wn.pdf")
type1.wn.plot
dev.off()

type1.min.plot <- dt.nmin%>%select(p_C, M2, type1.Wald, type1.FM, type1.WN)%>%
  melt(id.vars = c("p_C","M2"))%>%
  mutate(Method = case_when(variable == "type1.Wald" ~ "Wald",
                            variable == "type1.FM"   ~ "Farrington-Manning",
                            variable == "type1.WN"   ~ "Wilson-Newcombe"))%>%
  select(-variable)%>%
  rename(type1 = value)

type1.min.plott <- type1.min.plot%>%ggplot(aes(x=p_C, y=type1, group=Method))+
  geom_point(aes(color=Method))+
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2)+
  theme_bw()+
  #scale_x_continuous(labels = as.character(p_C), breaks = p_C)+
  scale_y_continuous(limits = c(0,0.05))+
  xlab("Proportion of events in active treatment")+
  ylab("Type-I error")+
  theme(legend.position = "bottom")+
  ggtitle("Type-I errors for 10000 simulations and Nmin")+
  facet_wrap(~M2)

pdf("full_nmin_type1_10000.pdf")
type1.min.plott
dev.off()

type1.max.plot <- dt.nmax%>%select(p_C, M2, type1.Wald, type1.FM, type1.WN)%>%
  melt(id.vars = c("p_C","M2"))%>%
  mutate(Method = case_when(variable == "type1.Wald" ~ "Wald",
                            variable == "type1.FM"   ~ "Farrington-Manning",
                            variable == "type1.WN"   ~ "Wilson-Newcombe"))%>%
  select(-variable)%>%
  rename(type1 = value)

type1.max.plott <- type1.max.plot%>%ggplot(aes(x=p_C, y=type1, group=Method))+
  geom_point(aes(color=Method))+
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2)+
  theme_bw()+
  #scale_x_continuous(labels = as.character(p_C), breaks = p_C)+
  scale_y_continuous(limits = c(0,0.05))+
  xlab("Proportion of events in active treatment")+
  ylab("Type-I error")+
  theme(legend.position = "bottom")+
  ggtitle("Type-I errors for 10000 simulations and Nmax")+
  facet_wrap(~M2)

pdf("full_nmax_type1_10000.pdf")
type1.max.plott
dev.off()

####################
### Power Each  ###
####################

dt.full.sum <- readRDS('dtfullsum_each.rds')

pow1 <- ggplot(data=dt.full.sum, aes(x=power.Wald, y=power.FM))+ 
  geom_point()+
  geom_abline()+
  theme_bw()+
  xlab("Empirical Power Wald")+
  ylab("Empirical Power Farrington-Manning")+
  ggtitle("Empirical power Wald vs FM - 10000 simulations") +
  scale_x_continuous(limits=c(0.85,.95))+
  scale_y_continuous(limits=c(0.85,.95))
  
pow2 <- ggplot(data=dt.full.sum, aes(x=power.Wald, y=power.WN))+ 
  geom_point()+
  geom_abline()+
  theme_bw()+
  xlab("Empirical Power Wald")+
  ylab("Empirical Power Newcombe-Wilson")+
  ggtitle("Empirical power Wald vs NW - 10000 simulations") +
  scale_x_continuous(limits=c(0.85,.95))+
  scale_y_continuous(limits=c(0.85,.95))

pow3 <- ggplot(data=dt.full.sum, aes(x=power.FM, y=power.WN))+ 
  geom_point()+
  geom_abline()+
  theme_bw()+
  xlab("Empirical Farrington-Manning")+
  ylab("Empirical Power Newcombe-Wilson")+
  ggtitle("Empirical power Wald vs NW - 10000 simulations") +
  scale_x_continuous(limits=c(0.85,.95))+
  scale_y_continuous(limits=c(0.85,.95))

pdf("fullpower1_each_10000.pdf")
pow1
dev.off()

pdf("fullpower2_each_10000.pdf")
pow2
dev.off()

pdf("fullpower3_each_10000.pdf")
pow3
dev.off()



###################################
### Type-I Error - N Each 10000 ###
##################################

dt.full.sum <- readRDS('DataSummaries/dtfullsum_each.rds')

type1.full.plot <- dt.full.sum%>%select(p_C, M2, type1.Wald, type1.FM, type1.WN)%>%
  melt(id.vars = c("p_C","M2"))%>%
  mutate(Method = case_when(variable == "type1.Wald" ~ "Wald",
                            variable == "type1.FM"   ~ "Farrington-Manning",
                            variable == "type1.WN"   ~ "Newcombe"))%>%
  select(-variable)%>%
  rename(type1 = value)%>%
  mutate(M21 = sprintf("Delta ~ %g",M2))

type1.full.plot.res <- 
  ggplot(data = type1.full.plot, aes(x=p_C, y=type1, group=Method))+
  geom_point(aes(color=Method))+
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2)+
  theme_bw()+
  #scale_x_continuous(labels = as.character(p_C), breaks = p_C)+
  scale_y_continuous(limits = c(0,0.05))+
  xlab("Proportion of events in active treatment")+
  ylab("Type-I error")+
  theme(legend.position = "bottom",
        text = element_text(size = 13),
        strip.text = element_text(size = rel(0.7))) +
  facet_wrap(~M21, labeller = label_parsed)

temp <- plotly::ggplotly(type1.full.plot.res)

htmlwidgets::saveWidget(temp,"temp.html")  

pdf("../../Proposal/full_each_type1_10000.pdf")
type1.full.plot.res
dev.off()


############################
## SS per method ###########
############################


dt.full.sum <- readRDS('DataSummaries/dtfullsum_each.rds')

ss.full.plot <- dt.full.sum%>%select(p_C, M2, N.Wald, N.FM, N.WN)%>%
  melt(id.vars = c("p_C","M2"))%>%
  mutate(Method = case_when(variable == "N.Wald" ~ "Wald",
                            variable == "N.FM"   ~ "Farrington-Manning",
                            variable == "N.WN"   ~ "Wilson-Newcombe"))%>%
  select(-variable)%>%
  rename(ss = value)

ss.full.plot.res <- ggplot(data = ss.full.plot, aes(x=p_C, y=ss, fill=Method))+
  geom_bar(stat = 'identity', position=position_dodge())+
  #geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2)+
  theme_bw()+
  #scale_x_continuous(labels = as.character(p_C), breaks = p_C)+
  #scale_y_continuous(limits = c(0,0.05))+
  xlab("Proportion of events in active treatment")+
  ylab("Sample size")+
  theme(legend.position = "bottom",
        text = element_text(size = 13),
        strip.text = element_text(size = rel(0.7)))+
  ggtitle("Samaple size per scenario, per method")+
  facet_wrap(~M2)

pdf("ss_perscenario.pdf")
ss.full.plot.res
dev.off()


dt.ss.diff <-dt.full.sum%>%
  mutate(N.Wald.FM=abs(N.Wald-N.FM),
         N.Wald.WN=abs(N.Wald-N.WN),
         N.FM.WN=abs(N.WN-N.FM))

ss.full.plot.diff <- dt.ss.diff%>%select(p_C, M2, N.Wald.FM, N.Wald.WN, N.FM.WN)%>%
  melt(id.vars = c("p_C","M2"))%>%
  mutate(Method = case_when(variable == "N.Wald.FM" ~ "Wald vs FM",
                            variable == "N.Wald.WN" ~ "Wald vs WN",
                            variable == "N.FM.WN"   ~ "FM vs WN"))%>%
  select(-variable)%>%
  rename(ss = value)

ss.full.plot.res.diff <- ggplot(data = ss.full.plot.diff, aes(x=p_C, y=ss, fill=Method))+
  geom_bar(stat = 'identity', position=position_dodge())+
  #geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2)+
  theme_bw()+
  #scale_x_continuous(labels = as.character(p_C), breaks = p_C)+
  #scale_y_continuous(limits = c(0,0.05))+
  xlab("Proportion of events in active treatment")+
  ylab("Sample size differences")+
  theme(legend.position = "bottom")+
  ggtitle("Samaple size differences per scenario")+
  facet_wrap(~M2)

pdf("ss_perscenario_diff.pdf")
ss.full.plot.res.diff
dev.off()

###################################
### Type-I Error - Wald 20000 ###
##################################

dt.full.sum <- readRDS('dtfullsum_wald20000.rds')

type1.full.plot <- dt.full.sum%>%select(p_C, M2, type1.Wald)%>%
  melt(id.vars = c("p_C","M2"))%>%
  mutate(Method = case_when(variable == "type1.Wald" ~ "Wald"))%>%
  select(-variable)%>%
  rename(type1 = value)

type1.full.plot.res <- ggplot(data = type1.full.plot, aes(x=p_C, y=type1, group=Method))+
  geom_point(aes(color=Method))+
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2)+
  theme_bw()+
  #scale_x_continuous(labels = as.character(p_C), breaks = p_C)+
  scale_y_continuous(limits = c(0,0.05))+
  xlab("Proportion of events in active treatment")+
  ylab("Type-I error")+
  theme(legend.position = "bottom")+
  ggtitle("Type-I errors for 20000 simulations")+
  facet_wrap(~M2)

pdf("full_wald_type1_20000.pdf")
type1.full.plot.res
dev.off()

#####################################################
#### CCA Analysis Graphs - Type I Error #############
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

dt.full2 <- dt.full1%>%
  dplyr::mutate(MISSING='mcar')%>%
  dplyr::bind_rows(dt.full1%>%
                     dplyr::mutate(MISSING='mar'),
                   dt.full1%>%
                     dplyr::mutate(MISSING='mnar') )


cca <- readRDS('DataSummaries/cca.rds')

cca.full <- cca%>%
  dplyr::rename(p_C=p_T)%>%
  dplyr::left_join(dt.full1%>%
                      dplyr::select(scenario.id, TYPE, type1full), by = c('scenario.id','TYPE'))

#CCA Wald MCAR
type1.gdt <- cca.full%>%
  dplyr::filter(TYPE=='wald', MISSING=='mcar')%>%
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
  ggtitle("Type I Error: Wald, MCAR, CCA Analysis")
  

pdf("final_outputs/type1_ccawaldmcar.pdf")
type1.g
dev.off()

#CCA Wald MAR
type1.gdt <- cca.full%>%
  dplyr::filter(TYPE=='wald', MISSING=='mar')%>%
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
  ggtitle("Type I: Wald, MAR, CCA Analysis New")


pdf("final_outputs/type1_ccawaldmarnew.pdf")
type1.g
dev.off()


#CCA Wald MNAR
type1.gdt <- cca.full%>%
  dplyr::filter(TYPE=='wald', MISSING=='mnar')%>%
  dplyr::mutate(scenario.idc = sprintf('p=%s & M=%s',p_C,M2))


type1.g <- type1.gdt%>%
  ggplot2::ggplot(aes(PERCENT, type1)) +
  geom_segment(aes(x = PERCENT, y = type1full, xend = PERCENT, yend = type1), colour="red") +
  geom_point(size=0.5) +
  geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2) +
  scale_y_continuous(limits = c(0,0.15)) +
  facet_wrap(~ scenario.idc) +
  theme_bw() +
  xlab("% of Missing Observations")+
  ylab("Type-I error")+
  ggtitle("Type I Error: Wald, MNAR, CCA Analysis New")


pdf("final_outputs/type1_ccawaldmnarnew.pdf")
type1.g
dev.off()


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



