library(dplyr)
library(purrr)
library(ggplot2)

source("cluster/pgms/init.R")

ss <- readRDS("cluster/ss.bounds.rds")

##############################################
## Type-I error for the fully observed data ##
##############################################

full.type1 <- map_df(list.files("cluster/out/overall", "full.type1", full.names = T), readRDS)

h0.full.all <-
  full.type1%>%
  ggplot(aes(x = p_C, y = reject.h0)) + 
  geom_point(aes(group = method, color = method, shape = method)) +
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) +
  scale_x_continuous(breaks = c(0.65, 0.75, 0.85), limits = c(0.6,0.9)) +
  scale_y_continuous(limits = c(0,0.035)) +
  facet_wrap(~M2) +
  theme_bw() +
  labs(y = "Empirical Type-I error",
       x = "p_C",
       title = "Empirical type-I error: fully observed data") +
  theme(legend.position = 'bottom')
  

pdf("cluster/out/overall/plots/h0_full_all.pdf")
h0.full.all
dev.off()

  
###############################################
## Type-I error/Bias for the incomplete data ##
###############################################

h0.sing <- map_df(list.files("cluster/out/overall", "h0.sing", full.names = T), readRDS)

h0.sing$missing.desc <- factor(h0.sing$missing.desc,levels=unique(h0.sing$missing.desc)[c(4,3,2,1,5,6,7,8,9,10)])

h0.sing <-
  h0.sing%>%
  left_join(ss%>%
              dplyr::filter(method=="wald")%>%
              dplyr::select(scenario.id, p_C, M2, n.arm), by = c("scenario.id"))%>%
    dplyr::mutate(flabel = sprintf('p[C]: %s, Delta: %s, n: %s',p_C, M2, n.arm))

h0.cca.mar<-
  h0.sing%>%
  dplyr::filter(strategy=="cca", grepl("mar",missing)>0)%>%
  ggplot(aes(y=missing.desc,x=type1,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3) + 
  geom_vline(xintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  labs(x = "Empirical Type-I error",
       y = "Drop-out difference (%C-%T)" ,
       title = "Empirical type-I error: CCA for MAR") +
  theme_bw() +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/h0_cca_mar.pdf")
h0.cca.mar
dev.off()


#Bias
bias.cca.mar<-
  h0.sing%>%
  dplyr::filter(strategy=="cca", grepl("mar",missing)>0)%>%
  ggplot(aes(y=missing.desc,x=mean.bias,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3) + 
  geom_vline(xintercept=c(-0.1,-.1),
             linetype=2) + 
  scale_x_continuous(limits = c(-0.15, 0.15)) +
  labs(x = "Mean relative bias",
       y = "Drop-out difference (%C-%T)" ,
       title = "Mean relative bias: CCA for MAR") +
  theme_bw() +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/bias_cca_mar.pdf")
bias.cca.mar
dev.off()

####MCAR for CCA
h0.cca.mcar<-
  h0.sing%>%
  dplyr::filter(strategy=="cca", grepl("mcar",missing)>0)%>%
  dplyr::left_join(full.type1%>%
              dplyr::select(scenario.id, method, reject.h0),
            by = c("scenario.id", "method"))%>%
  dplyr::mutate(diff.type1 = reject.h0 - type1)
  



####MNAR
h0.mice <- map_df(list.files("cluster/out/overall", "h0.mice", full.names = T), readRDS)

h0.mnar<-
  h0.sing%>%
  dplyr::filter(strategy=="cca", grepl("mnar",missing)>0)%>%
  right_join(h0.mice%>%
               dplyr::rename(type1.mice = type1,
                            bias.mice = mean.bias)%>%
              dplyr::select(-c(do, missing.desc)),
            by = c("scenario.id", "method", "missing"))%>%
  ungroup()%>%
  dplyr::select(do, scenario.id, method, flabel, missing.desc, mean_pc, mean_pt, 
                type1, type1.mice, k.C.spec, k.T.spec, mean.bias, bias.mice)
 

wald.mnar1<-
  h0.mnar%>%
  dplyr::filter(method=="wald", missing.desc=="p_T_obs > p_T_full")

wald.mnar2<-
  h0.mnar%>%
  dplyr::filter(method=="wald", missing.desc!="p_T_obs > p_T_full")

h0.mnar%>%
  ggplot(aes(y=missing.desc,x=type1.mice,colour=method)) + 
  geom_point(aes(shape = method)) + 
  facet_wrap(~flabel,nrow=3, ncol = 3) + 
  geom_vline(xintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  labs(x = "Empirical Type-I error",
       y = "Drop-out difference (%C-%T)" ,
       title = "Empirical type-I error: Nested MICE for MNAR") +
  theme_bw() +
  theme(legend.position = 'bottom')

