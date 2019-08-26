library(dplyr)
library(purrr)
library(ggplot2)

source("cluster/pgms/init.R")

ss <- readRDS("cluster/ss.bounds.rds")

full.type1 <- map_df(list.files("cluster/out/overall", "full.type1", full.names = T), readRDS)

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

sc6_do20 <- readRDS("cluster/out/wn/2xcont/cont2xH0_wn_mice_sc6_do20_param1_newwn.rds")
sc6_do15 <- readRDS("cluster/out/wn/2xcont/do15/cont2xH0_wn_mice_sc6_do15_param1_newwn.rds")
sc6_do10 <- readRDS("cluster/out/wn/2xcont/do10/cont2xH0_wn_mice_sc6_do10_param1_newwn.rds")
sc6_do5  <- readRDS("cluster/out/wn/2xcont/do5/cont2xH0_wn_mice_sc6_do5_param1_newwn.rds")

sc_select <- h0.mnar%>%
  filter(scenario.id == 6,
         missing.desc == "p_T_obs > p_T_full",
         method == "wn")%>%
  mutate(type1.mice = case_when(do == 0.2 ~ h0.mice.sum.wn(sc6_do20)$type1,
                                do == 0.15 ~ h0.mice.sum.wn(sc6_do15)$type1,
                                do == 0.1  ~ h0.mice.sum.wn(sc6_do10)$type1,
                                do == 0.05 ~ h0.mice.sum.wn(sc6_do5)$type1),
         method = "wn-mi")
h0.mnar.new <-h0.mnar%>%
  bind_rows(sc_select)

h0.mice.mnar.sc6 <- 
  h0.mnar.new%>%
  dplyr::filter(scenario.id%in%c(6))%>%
  dplyr::filter(missing.desc=="p_T_obs > p_T_full")%>%
  ggplot(aes(x=do,y=type1.mice,colour=method)) + 
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  geom_point(size = 3) + 
  facet_wrap(~flabel,nrow=1, ncol = 1, labeller = ggplot2::label_parsed) + 
  scale_y_continuous(breaks = seq(0, 0.05, 0.01), limits = c(0, 0.06)) +
  scale_x_continuous(breaks = seq(0.05, 0.2, 0.05), limits = c(0, 0.25),
                     labels = scales::percent_format(accuracy = 1))+
  scale_color_discrete(labels=c('FM','Wald','WN', 'WN-MI')) + 
  labs(y = "Empirical Type-I error",
       x = "Drop-out rate (%)",
       color = "Method")  +
  theme(text=element_text(size=12))


pdf("cluster/out/overall/plots/h0_mice_mnar1_sc6.pdf")
h0.mice.mnar.sc6
dev.off()

