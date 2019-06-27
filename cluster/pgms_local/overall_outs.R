library(dplyr)
library(purrr)
library(ggplot2)

source("cluster/pgms/init.R")

ss <- readRDS("cluster/ss.bounds.rds")

##############################################
## Type-I error for the fully observed data ##
##############################################

full.type1 <- map_df(list.files("cluster/out/overall", "full.type1", full.names = T), readRDS)

#check phats for each scenario/method again
# check <- full.type1%>%
#   dplyr::mutate(pc.check = round(C_phat - p_C, 3),
#                 pt.check = round(T_phat - p_C + M2, 3),
#                 m2.check = round(C_phat - T_phat - M2, 3))

h0.full.all <-
  full.type1%>%
  ggplot(aes(x = p_C, y = reject.h0)) + 
  geom_point(aes(group = method, color = method, shape = method)) +
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) +
  scale_x_continuous(breaks = seq(0.6, 0.95, 0.1), limits = c(0.55,1)) +
  scale_y_continuous(limits = c(0,0.05)) +
  facet_wrap(~M2) +
  theme_bw() +
  labs(y = "Empirical Type-I error",
       x = "Event probability in control group") +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/h0_full_all.pdf")
h0.full.all
dev.off()


h0.full.all9 <-
  full.type1%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  ggplot(aes(x = p_C, y = reject.h0)) + 
  geom_point(aes(group = method, color = method, shape = method)) +
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) +
  scale_x_continuous(breaks = c(0.65, 0.75, 0.85), limits = c(0.6,0.9)) +
  scale_y_continuous(limits = c(0,0.05)) +
  facet_wrap(~M2) +
  theme_bw() +
  labs(y = "Empirical Type-I error",
       x = "Event probability in control group") +
  theme(legend.position = 'bottom')
  

pdf("cluster/out/overall/plots/h0_full_all9.pdf")
h0.full.all9
dev.off()

  
###############################################
## Type-I error/Bias for the incomplete data ##
###############################################

#######################
##### CCA for MAR #####
#######################

h0.sing <- map_df(list.files("cluster/out/overall", "h0.sing", full.names = T), readRDS)

#h0.sing$missing.desc <- factor(h0.sing$missing.desc,levels=unique(h0.sing$missing.desc)[c(4,3,2,1,5,6,7,8,9,10)])
h0.sing$missing.desc <- factor(h0.sing$missing.desc,levels=unique(h0.sing$missing.desc)[c(9,7,2,1,3,8,10,4,5,6)])

h0.sing <-
  h0.sing%>%
  left_join(ss%>%
              dplyr::filter(method=="wald")%>%
              dplyr::select(scenario.id, p_C, M2, n.arm), by = c("scenario.id"))%>%
    dplyr::mutate(flabel = sprintf('Delta:%s~p[C]:%s~n:%s', M2, p_C, n.arm))%>%
  left_join(full.type1%>%
              dplyr::select(scenario.id, method, reject.h0), 
            by = c("scenario.id", "method")) 

h0.cca.mar.20<-
  h0.sing%>%
  #dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(strategy=="cca", grepl("mar",missing)>0, do==0.2)%>%
  ggplot(aes(y=missing.desc,x=type1,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=5, ncol = 6, labeller = ggplot2::label_parsed) + 
  geom_vline(xintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  scale_x_continuous(breaks = seq(0, 0.045, 0.01), limits = c(0, 0.045)) +
  labs(x = "Empirical Type-I error",
       y = "Drop-out difference (%C-%T)") +
  theme_bw() +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/h0_cca_mar_do20.pdf")
h0.cca.mar.20
dev.off()

h0.cca.mar.15<-
  h0.sing%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(strategy=="cca", grepl("mar",missing)>0, do==0.15)%>%
  ggplot(aes(y=missing.desc,x=type1,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_vline(xintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  scale_x_continuous(breaks = seq(0, 0.045, 0.01), limits = c(0, 0.045)) +
  labs(x = "Empirical Type-I error",
       y = "Drop-out difference (%C-%T)") +
  theme_bw() +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/h0_cca_mar_do15.pdf")
h0.cca.mar.15
dev.off()

h0.cca.mar.10<-
  h0.sing%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(strategy=="cca", grepl("mar",missing)>0, do==0.10)%>%
  ggplot(aes(y=missing.desc,x=type1,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_vline(xintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  scale_x_continuous(breaks = seq(0, 0.045, 0.01), limits = c(0, 0.045)) +
  labs(x = "Empirical Type-I error",
       y = "Drop-out difference (%C-%T)") +
  theme_bw() +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/h0_cca_mar_do10.pdf")
h0.cca.mar.10
dev.off()

h0.cca.mar.blnc <-
h0.sing%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(strategy=="cca", missing%in%c("mar1","mar"))%>%
  ggplot(aes(x=do,y=type1,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  scale_y_continuous(breaks = seq(0, 0.035, 0.01), limits = c(0, 0.035)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1))+
  labs(y = "Empirical Type-I error",
       x = "Drop-out rate (%)") +
  theme_bw() +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/h0_cca_mar_blnc.pdf")
h0.cca.mar.blnc
dev.off()

##### Bias #####
bias.cca.mar.20<-
  h0.sing%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(strategy=="cca", grepl("mar",missing)>0, do==0.2)%>%
  ggplot(aes(y=missing.desc,x=mean.bias,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_vline(xintercept=c(-0.1,0.1),
             linetype=2) + 
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  labs(x = "Mean relative bias",
       y = "Drop-out difference (%C-%T)")+
  theme_bw() +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/bias_cca_mar_do20.pdf")
bias.cca.mar.20
dev.off()

bias.cca.mar.15<-
  h0.sing%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(strategy=="cca", grepl("mar",missing)>0, do==0.15)%>%
  ggplot(aes(y=missing.desc,x=mean.bias,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_vline(xintercept=c(-0.1,0.1),
             linetype=2) + 
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  labs(x = "Mean relative bias",
       y = "Drop-out difference (%C-%T)")+
  theme_bw() +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/bias_cca_mar_do15.pdf")
bias.cca.mar.15
dev.off()

bias.cca.mar.10<-
  h0.sing%>%
  dplyr::filter(strategy=="cca", grepl("mar",missing)>0, do==0.10)%>%
  ggplot(aes(y=missing.desc,x=mean.bias,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_vline(xintercept=c(-0.1,0.1),
             linetype=2) + 
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  labs(x = "Mean relative bias",
       y = "Drop-out difference (%C-%T)")+
  theme_bw() +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/bias_cca_mar_do10.pdf")
bias.cca.mar.10
dev.off()

bias.cca.mar.blnc <-
  h0.sing%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(strategy=="cca", missing%in%c("mar1","mar"))%>%
  ggplot(aes(x=do,y=mean.bias,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_hline(yintercept=c(-0.1,0.1),
             linetype=2) + 
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1))+
  labs(y = "Mean relative bias",
       x = "Drop-out rate (%)") +
  theme_bw() +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/bias_cca_mar_blnc.pdf")
bias.cca.mar.blnc
dev.off()

###################################
##### Best/Worst/CCA for MCAR #####
###################################


h0.best.mcar<-
  h0.sing%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(strategy=="best", missing=="mcar")%>%
  ggplot(aes(x=do,y=type1,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  #scale_y_continuous(breaks = seq(0, 0.045, 0.01), limits = c(0, 0.045)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Empirical Type-I error",
       x = "Drop-out rate (%)") +
  theme_bw() +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/h0_best_mcar.pdf")
h0.best.mcar
dev.off()

h0.worst.mcar<-
  h0.sing%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(strategy=="worst", missing=="mcar")%>%
  ggplot(aes(x=do,y=type1,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  #scale_y_continuous(breaks = seq(0, 0.045, 0.01), limits = c(0, 0.045)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Empirical Type-I error",
       x = "Drop-out rate (%)") +
  theme_bw() +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/h0_worst_mcar.pdf")
h0.worst.mcar
dev.off()


h0.cca.mcar<-
  h0.sing%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(strategy=="cca", missing=="mcar")%>%
  ggplot(aes(x=do,y=type1,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  scale_y_continuous(breaks = seq(0, 0.045, 0.01), limits = c(0, 0.045)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Empirical Type-I error",
       x = "Drop-out rate (%)") +
  theme_bw() +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/h0_cca_mcar.pdf")
h0.cca.mcar
dev.off()


###########################
##### CCA/MI for MNAR #####
###########################

h0.mice <- map_df(list.files("cluster/out/overall", "h0.mice", full.names = T), readRDS)

h0.mnar<-
  h0.sing%>%
  dplyr::filter(strategy=="cca", grepl("mnar",missing)>0)%>%
  left_join(h0.mice%>%
               dplyr::rename(type1.mice = type1,
                            bias.mice = mean.bias)%>%
              dplyr::select(-c(missing.desc)),
            by = c("scenario.id", "method", "missing", "do"))%>%
  ungroup()%>%
  dplyr::select(do, scenario.id, method, flabel, missing.desc, mean_pc, mean_pt, 
                type1, type1.mice, k.C.spec, k.T.spec, mean.bias, bias.mice)%>%
  dplyr::mutate(flabel1 = sprintf('%s~k[T]:%s', flabel,gsub('normal','N',k.T.spec)),
                flabel2 = sprintf('%s~k[C]:%s', flabel,gsub('normal','N',k.C.spec)))
 

h0.cca.mnar1 <- 
h0.mnar%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(missing.desc=="p_T_obs > p_T_full")%>%
  ggplot(aes(x=do,y=type1,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_hline(yintercept=c(0.9,1.1)*0.025,
              linetype=2) + 
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1))+
  labs(y = "Empirical Type-I error",
       x = "Drop-out rate (%)") +
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.text.x = element_text(angle = 0, size = rel(0.9)))

pdf("cluster/out/overall/plots/h0_cca_mnar1.pdf")
h0.cca.mnar1
dev.off()

bias.cca.mnar1 <-
h0.mnar%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(missing.desc=="p_T_obs > p_T_full")%>%
  ggplot(aes(x=do,y=mean.bias,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_hline(yintercept=c(-0.1,0.1),
             linetype=2) + 
  #scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1))+
  labs(y = "Empirical Type-I error",
       x = "Drop-out rate (%)") +
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.text.x = element_text(angle = 0, size = rel(0.9)))

pdf("cluster/out/overall/plots/bias_cca_mnar1.pdf")
bias.cca.mnar1
dev.off()

h0.cca.mnar2 <- 
  h0.mnar%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(missing.desc!="p_T_obs > p_T_full")%>%
  ggplot(aes(x=do,y=type1,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1))+
  labs(y = "Empirical Type-I error",
       x = "Drop-out rate (%)") +
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.text.x = element_text(angle = 0, size = rel(0.9)))

pdf("cluster/out/overall/plots/h0_cca_mnar2.pdf")
h0.cca.mnar2
dev.off()

bias.cca.mnar2 <-
  h0.mnar%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(missing.desc!="p_T_obs > p_T_full")%>%
  ggplot(aes(x=do,y=mean.bias,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_hline(yintercept=c(-0.1,0.1),
             linetype=2) + 
  #scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1))+
  labs(y = "Empirical Type-I error",
       x = "Drop-out rate (%)") +
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.text.x = element_text(angle = 0, size = rel(0.9)))

pdf("cluster/out/overall/plots/bias_cca_mnar2.pdf")
bias.cca.mnar2
dev.off()

h0.mice.mnar1 <- 
  h0.mnar%>%
 # dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(missing.desc=="p_T_obs > p_T_full", is.na(type1.mice)==F)%>%
  ggplot(aes(x=do,y=type1.mice,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel1,nrow=5, ncol = 6,labeller = ggplot2::label_parsed) + 
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  scale_y_continuous(breaks = seq(0, 0.05, 0.01), limits = c(0, 0.06)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1))+
  labs(y = "Empirical Type-I error",
       x = "Drop-out rate (%)") +
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.text.x = element_text(angle = 0, size = rel(0.9)))

pdf("cluster/out/overall/plots/h0_mice_mnar1.pdf")
h0.mice.mnar1
dev.off()

h0.mice.mnar2 <- 
  h0.mnar%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(missing.desc!="p_T_obs > p_T_full", is.na(type1.mice)==F)%>%
  ggplot(aes(x=do,y=type1.mice,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel2,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  scale_y_continuous(breaks = seq(0, 0.05, 0.01), limits = c(0, 0.06)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1))+
  labs(y = "Empirical Type-I error",
       x = "Drop-out rate (%)") +
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.text.x = element_text(angle = 0, size = rel(0.9)))

pdf("cluster/out/overall/plots/h0_mice_mnar2.pdf")
h0.mice.mnar2
dev.off()

bias.mice.mnar1 <- 
  h0.mnar%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(missing.desc=="p_T_obs > p_T_full", is.na(type1.mice)==F)%>%
  ggplot(aes(x=do,y=bias.mice,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel1,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_hline(yintercept=c(-0.1,0.1),
             linetype=2) + 
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1))+
  labs(y = "Mean relative bias",
       x = "Drop-out rate (%)") +
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.text.x = element_text(angle = 0, size = rel(0.9)))

pdf("cluster/out/overall/plots/bias_mice_mnar1.pdf")
bias.mice.mnar1
dev.off()

bias.mice.mnar2 <- 
  h0.mnar%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(missing.desc!="p_T_obs > p_T_full", is.na(type1.mice)==F)%>%
  ggplot(aes(x=do,y=bias.mice,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel2,nrow=3, ncol = 3, labeller = ggplot2::label_parsed) + 
  geom_hline(yintercept=c(-0.1,0.1),
             linetype=2) + 
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25), 
                     labels = scales::percent_format(accuracy = 1))+
  labs(y = "Mean relative bias",
       x = "Drop-out rate (%)") +
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.text.x = element_text(angle = 0, size = rel(0.9)))

pdf("cluster/out/overall/plots/bias_mice_mnar2.pdf")
bias.mice.mnar2
dev.off()
