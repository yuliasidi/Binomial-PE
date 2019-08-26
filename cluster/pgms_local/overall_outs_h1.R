library(dplyr)
library(purrr)
library(ggplot2)

source("cluster/pgms/init.R")

ss <- readRDS("cluster/ss.bounds.rds")

##############################################
## Power for the fully observed data        ##
##############################################

full.power <- map_df(list.files("cluster/out/overall", "full.power", full.names = T), readRDS)

full.power <- full.power%>%
  dplyr::mutate(flabel = sprintf('Delta:%s', M2))
  
#check phats for each scenario/method again
check <- full.power%>%
  dplyr::mutate(pc.check = round(C_phat - p_C, 3),
                pt.check = round(T_phat - p_C, 3),
                m2.check = round(C_phat - T_phat, 3))

h1.full.all <-
  full.power%>%
  ggplot(aes(x = p_C, y = reject.h0)) + 
  geom_point(aes(group = method, color = method), size = 3) +
  geom_hline(yintercept=0.9,
             linetype=2) +
  scale_x_continuous(breaks = seq(0.6, 0.95, 0.1), limits = c(0.55,1)) +
  scale_y_continuous(limits = c(0.885,0.915)) +
  facet_wrap(~flabel, labeller = ggplot2::label_parsed) +
  scale_color_discrete(labels=c('FM','Wald','WN')) + 
  scale_shape_discrete(labels=c('FM','Wald','WN')) + 
  labs(y = "Empirical Power",
       x = "Event probability in control group",
       colour='Method')
  
pdf("cluster/out/overall/plots/h1_full_all.pdf")
h1.full.all
dev.off()

###############################################
##        Power for the incomplete data      ##
###############################################

#######################
##### CCA for MAR #####
#######################

h1.sing <- map_df(list.files("cluster/out/overall", "h1.sing", full.names = T), readRDS)

#h0.sing$missing.desc <- factor(h0.sing$missing.desc,levels=unique(h0.sing$missing.desc)[c(4,3,2,1,5,6,7,8,9,10)])
h1.sing$missing.desc <- factor(h1.sing$missing.desc,levels=unique(h1.sing$missing.desc)[c(7,3,2,1,8,9,10,4,5,6)])

h1.sing <-
  h1.sing%>%
  left_join(ss%>%
              dplyr::filter(method=="wald")%>%
              dplyr::select(scenario.id, p_C, M2, n.arm), by = c("scenario.id"))%>%
    dplyr::mutate(flabel = sprintf('Delta:%s~p[C]:%s~n:%s', M2, p_C, n.arm))%>%
  left_join(full.power%>%
              dplyr::select(scenario.id, method, reject.h0), 
            by = c("scenario.id", "method")) 

##################################################
# Power CCA, MAR, balanced do, only 9 scenarios # 
##################################################

h1.cca.mar.blnc <-
h1.sing%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(strategy=="cca", missing%in%c("mar1","mar"))%>%
  ggplot(aes(x=do,y=power,colour=method)) + 
  geom_hline(yintercept=0.9,
             linetype=2) + 
  geom_point(size = 3) + 
  facet_wrap(~flabel,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  scale_y_continuous(breaks = seq(0.8, 0.95, 0.05), limits = c(0.8, 0.95)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1))+
  scale_color_discrete(labels=c('FM','Wald','WN')) + 
  labs(y = "Empirical power",
       x = "Drop-out rate (%)",
       color = "Method") +
  theme(text=element_text(size=12))


pdf("cluster/out/overall/plots/h1_cca_mar_blnc.pdf")
h1.cca.mar.blnc
dev.off()

##############################################
# Power CCA, MAR, balanced do, 30 scenarios # 
##############################################

h1.cca.mar.blnc.sc.all <-
  h1.sing%>%
  dplyr::filter(strategy=="cca", missing%in%c("mar1","mar"))%>%
  ggplot(aes(x=do,y=power,colour=method)) + 
  geom_hline(yintercept=0.9,
             linetype=2) + 
  geom_point(size = 2) + 
  facet_wrap(~flabel, nrow = 5, ncol = 6, labeller = ggplot2::label_parsed) + 
  scale_y_continuous(breaks = seq(0.8, 0.95, 0.05), limits = c(0.8, 0.95)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1))+
  scale_color_discrete(labels=c('FM','Wald','WN')) + 
  labs(y = "Empirical power",
       x = "Drop-out rate (%)",
       color = "Method") +
  theme(text=element_text(size=8))

pdf("cluster/out/overall/plots/h1_cca_mar_blnc_scall.pdf")
h1.cca.mar.blnc.sc.all
dev.off()

#######################################################
##### Power CCA for MCAR, 9 Scenarios             #####
#######################################################


h1.cca.mcar<-
  h1.sing%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(strategy=="cca", missing=="mcar")%>%
  ggplot(aes(x=do,y=power,colour=method)) + 
  geom_point(size = 3) + 
  facet_wrap(~flabel,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_hline(yintercept=0.9,
             linetype=2) + 
  scale_y_continuous(breaks = seq(0.8, 0.95, 0.05), limits = c(0.8, 0.95)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_color_discrete(labels=c('FM','Wald','WN')) + 
  labs(y = "Empirical power error",
       x = "Drop-out rate (%)",
       color = "Method") +
  theme(text=element_text(size=12))


pdf("cluster/out/overall/plots/h1_cca_mcar.pdf")
h1.cca.mcar
dev.off()

#######################################################
##### Power CCA for MCAR, 30 Scenarios            #####
#######################################################


h1.cca.mcar.sc.all<-
  h1.sing%>%
  dplyr::filter(strategy=="cca", missing=="mcar")%>%
  ggplot(aes(x=do,y=power,colour=method)) + 
  geom_point(size = 2) + 
  facet_wrap(~flabel,nrow=5, ncol = 6, labeller = ggplot2::label_parsed) + 
  geom_hline(yintercept=0.9,
             linetype=2) + 
  scale_y_continuous(breaks = seq(0.8, 0.95, 0.05), limits = c(0.8, 0.95)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_color_discrete(labels=c('FM','Wald','WN')) + 
  labs(y = "Empirical power error",
       x = "Drop-out rate (%)",
       color = "Method") +
  theme(text=element_text(size=8))


pdf("cluster/out/overall/plots/h1_cca_mcar_scall.pdf")
h1.cca.mcar.sc.all
dev.off()



###########################
##### MI for MNAR     #####
###########################

h1.mice <- map_df(list.files("cluster/out/overall", "h1.mice", full.names = T), readRDS)

h1.mnar<-
  h1.sing%>%
  dplyr::filter(strategy=="cca", grepl("mnar",missing)>0)%>%
  left_join(h1.mice%>%
               dplyr::rename(power.mice = power)%>%
              dplyr::select(-c(missing.desc)),
            by = c("scenario.id", "method", "missing", "do"))%>%
  ungroup()%>%
  dplyr::select(do, scenario.id, method, flabel, missing.desc, mean_pc, mean_pt, 
                power, power.mice, k.C.spec, k.T.spec)%>%
  dplyr::mutate(flabel1 = sprintf('%s~k[T]:%s', flabel,gsub('normal','N',k.T.spec)),
                flabel2 = sprintf('%s~k[C]:%s', flabel,gsub('normal','N',k.C.spec)))
 

#####################################
# Power MICE, MNAR only 9 scenarios # 
#####################################

h1.mice.mnar1 <- 
h1.mnar%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(missing.desc=="p_T_obs > p_T_full")%>%
  #dplyr::filter(is.na(power.mice)==F)%>%
  ggplot(aes(x=do,y=power.mice,colour=method)) + 
  geom_hline(yintercept = 0.9,
             linetype = 2) + 
  geom_point(size = 3) + 
  facet_wrap(~flabel, nrow = 3, ncol = 3, labeller = ggplot2::label_parsed) + 
  scale_y_continuous(breaks = seq(0.55, 1, 0.1), limits = c(0.55, 1)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1))+
  scale_color_discrete(labels=c('FM','Wald','WN')) + 
  labs(y = "Empirical power",
       x = "Drop-out rate (%)",
       color = "Method")+
  theme(text=element_text(size=12))


pdf("cluster/out/overall/plots/h1_mice_mnar1.pdf")
h1.mice.mnar1
dev.off()

h1.mice.mnar2 <- 
  h1.mnar%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(missing.desc!="p_T_obs > p_T_full")%>%
  dplyr::filter(is.na(power.mice)==F)%>%
  ggplot(aes(x=do,y=power.mice,colour=method)) + 
  geom_hline(yintercept = 0.9,
             linetype = 2) + 
  geom_point(size = 3) + 
  facet_wrap(~flabel, nrow = 3, ncol = 3, labeller = ggplot2::label_parsed) + 
  scale_y_continuous(breaks = seq(0.65, 0.95, 0.1), limits = c(0.65, 0.95)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1))+
  scale_color_discrete(labels=c('FM','Wald','WN')) + 
  labs(y = "Empirical power",
       x = "Drop-out rate (%)",
       color = "Method")+
  theme(text=element_text(size=12))


pdf("cluster/out/overall/plots/h1_mice_mnar2.pdf")
h1.mice.mnar2
dev.off()


#####################################
# Power MICE, MNAR 30 scenarios     # 
#####################################

h1.mice.mnar1.sc.all <- 
  h1.mnar%>%
  dplyr::filter(missing.desc=="p_T_obs > p_T_full")%>%
  dplyr::filter(is.na(power.mice)==F)%>%
  ggplot(aes(x=do,y=power.mice,colour=method)) + 
  geom_hline(yintercept = 0.9,
             linetype = 2) + 
  geom_point(size = 2) + 
  facet_wrap(~flabel, nrow = 5, ncol = 6, labeller = ggplot2::label_parsed) + 
  scale_y_continuous(breaks = seq(0.55, 0.95, 0.1), limits = c(0.5, 1)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1))+
  scale_color_discrete(labels=c('FM','Wald','WN')) + 
  labs(y = "Empirical power",
       x = "Drop-out rate (%)",
       color = "Method")+
  theme(text=element_text(size=8))


pdf("cluster/out/overall/plots/h1_mice_mnar1_scall.pdf")
h1.mice.mnar1.sc.all
dev.off()

h1.mice.mnar2.sc.all <- 
  h1.mnar%>%
  dplyr::filter(missing.desc!="p_T_obs > p_T_full")%>%
  dplyr::filter(is.na(power.mice)==F)%>%
  ggplot(aes(x=do,y=power.mice,colour=method)) + 
  geom_hline(yintercept = 0.9,
             linetype = 2) + 
  geom_point(size = 2) + 
  facet_wrap(~flabel, nrow = 5, ncol = 6, labeller = ggplot2::label_parsed) + 
  scale_y_continuous(breaks = seq(0.65, 0.95, 0.1), limits = c(0.6, 1)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1))+
  scale_color_discrete(labels=c('FM','Wald','WN')) + 
  labs(y = "Empirical power",
       x = "Drop-out rate (%)",
       color = "Method")+
  theme(text=element_text(size=8))


pdf("cluster/out/overall/plots/h1_mice_mnar2_scall.pdf")
h1.mice.mnar2.sc.all
dev.off()
