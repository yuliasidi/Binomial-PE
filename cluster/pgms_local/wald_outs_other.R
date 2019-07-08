library(dplyr)
library(tidyr)
library(purrr)

 
ss <- readRDS("cluster/ss.bounds.rds")
ss <- ss%>%
  dplyr::filter(method == "wald")

source("cluster/pgms/init.R")
source("funs/full.check.R")
source("funs/do.check.R")
source("funs/h0.sing.sum.R")
source("funs/h0.mice.sum.R")
source("funs/plot.type1.R")
source("funs/plot.bias.R")
source("funs/plot.power.R")
source("funs/miss.desc.R")
source("funs/missing.desc.adj.R")
source("funs/pcheck.cca.R") 




##### Different k's ############

x1.sc21.sing1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc21_do20_param1.rds")
x1.sc21.sing2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_sing_sc21_do20_param11.rds")
x1.sc21.sing <- append(x1.sc21.sing1,x1.sc21.sing2)
remove(x1.sc21.sing1, x1.sc21.sing2)

x1.sc21.mice1 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc21_do20_param1.rds")
x1.sc21.mice2 <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc21_do20_param11.rds")
x1.sc21.mice <- append(x1.sc21.mice1,x1.sc21.mice2)
remove(x1.sc21.mice1, x1.sc21.mice2)

x1.sc21.mice.k1 <- readRDS("cluster/out/wald/2xcont/change_k/cont2xH0_wald_mice_sc21_do20_param1_k1.rds")
x1.sc21.mice.k2 <- readRDS("cluster/out/wald/2xcont/change_k/cont2xH0_wald_mice_sc21_do20_param1_k2.rds")
x1.sc21.mice.k3 <- readRDS("cluster/out/wald/2xcont/change_k/cont2xH0_wald_mice_sc21_do20_param1_k3.rds")
x1.sc21.mice.k4 <- readRDS("cluster/out/wald/2xcont/change_k/cont2xH0_wald_mice_sc21_do20_param1_k4.rds")

diffk.wald.sc21.do20 <-
  bind_rows(
    h0.mice.sum(x1.sc21.mice),
    h0.mice.sum(x1.sc21.mice.k1),
    h0.mice.sum(x1.sc21.mice.k2),
    h0.mice.sum(x1.sc21.mice.k3),
    h0.mice.sum(x1.sc21.mice.k4))%>%
  dplyr::rename(type1.mice = type1,
                bias.mice = mean.bias)%>%
  dplyr::select(-c(missing.desc))%>%
  dplyr::right_join(h0.sing.sum(x1.sc21.sing)%>%
                      dplyr::filter(grepl("mnar", missing)>0, strategy == "cca"),
                    by = c("scenario.id", "missing", "do"))

diffk.wald.sc21.do20 <-
  diffk.wald.sc21.do20%>%
  dplyr::mutate(k.T = gsub('normal','N',k.T.spec),
                k.C = gsub('normal','N',k.C.spec))


library(ggplot2)

diffk.mnar1.wald.sc21.do20 <-
  diffk.wald.sc21.do20%>%
  dplyr::filter(missing.desc=="p_T_obs > p_T_full")%>%
  ggplot(aes(x=k.T,y=type1.mice)) + 
  geom_point(size = 3) +
  geom_hline(yintercept=diffk.wald.sc21.do20$type1[1]) + 
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  scale_y_continuous(breaks = seq(0, 0.23, 0.025), limits = c(0, .23),labels = c(seq(0, 0.20, 0.025),'CCA')) +
  labs(y = "Empirical Type-I error",
       x = latex2exp::TeX('k_T Distribution')) +
  theme(axis.text.x = element_text(size = rel(0.75)))
    


pdf("cluster/out/overall/plots/diffk_mnar1_wald_sc21_do20.pdf")
diffk.mnar1.wald.sc21.do20
dev.off()

diffk.mnar2.wald.sc21.do20 <-
  diffk.wald.sc21.do20%>%
  dplyr::filter(missing.desc!="p_T_obs > p_T_full")%>%
  ggplot(aes(x=k.C,y=type1.mice)) + 
  geom_point() + 
  geom_hline(yintercept=diffk.wald.sc21.do20$type1[10]) + 
  geom_hline(yintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  scale_y_continuous(breaks = seq(0, 0.15, 0.025), limits = c(0, .15)) +
  labs(y = "Empirical Type-I error",
       x = "k_C Distribution") +
  theme_bw() +
  theme(legend.position = 'bottom')

pdf("cluster/out/overall/plots/diffk_mnar2_wald_sc21_do20.pdf")
diffk.mnar2.wald.sc21.do20
dev.off()

#########################################
# MICE without nesting for scenario 21 ##
#########################################

x1.sc21.mice.nonest <- readRDS("cluster/out/wald/2xcont/cont2xH0_wald_mice_sc21_do20_nonest.rds")

x1.nonest.ex <-
  x1.sc21.mice.nonest%>%
  purrr::map_df(.f=function(x) x$ci.miss,.id = 'sim')%>%
  unnest()%>%
  unnest()%>%
  mutate(bias = round((qbar-M2)/M2,4))%>%
  dplyr::group_by(scenario.id, strategy, missing)%>%
  dplyr::summarise(type1=mean(reject.h0), mean.bias = mean(bias))%>%
  miss.desc()%>%
  dplyr::mutate(do = 0.2)%>%
  bind_rows(
    h0.sing.sum(x1.sc21.sing)%>%
      filter(grepl("mnar", missing)>0, strategy=="cca")%>%
      dplyr::select(-c(mean_pc, mean_pt))
  )

saveRDS(x1.nonest.ex, "cluster/out/wald/2xcont/nonest/sc21_do20_nonest.rds")
