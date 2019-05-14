k<-readRDS("cluster/out/fm/outH0_set1_p30_fm21_20.rds")
k1<-readRDS("cluster/out/fm/outH0_set1_m30_fm21_20.rds")

k2<-readRDS("cluster/out/fm/outH0_set1_m30_fm21_20_perarm.rds")

k3<-readRDS("cluster/out/fm/outH0_m30_fm21_20_set1_mi50.rds")
k4<-readRDS("cluster/out/fm/outH0_set1_p30_fm21_20_perarm.rds")
k5<-readRDS("cluster/out/fm/outH0_m30_fm21_20_set1_mi10.rds")
k6<-readRDS("cluster/out/fm/outH0_p30_fm21_20_set1_mi10.rds")
k7<-readRDS("cluster/out/fm/outH0_p30_fm21_20_set1_mi5.rds")
k8<-readRDS("cluster/out/fm/outH0_m30_fm21_20_set1_mi100.rds")
k9<-readRDS("cluster/out/fm/outH0_m30_fm21_20_set1_mi5.rds")
k10<-readRDS("cluster/out/fm/outH0_p30_fm21_20_set1_perarm_mi5.rds")
k11<-readRDS("cluster/out/fm/outH0_m30_fm21_20_set1_perarm_mi5.rds")






t1.H0 <- bind_rows(k%>%
                     mutate(rho = 0.3), 
                   k1%>%
                     mutate(rho = -0.3),
                   k2%>%
                     filter(strategy=="mice m=20")%>%
                     mutate(rho = -0.3, strategy="mice m=20 per arm"),
                   bind_rows(k3)%>%
                     filter(strategy=="mice m=50")%>%
                     mutate(rho = -0.3),
                   k4%>%
                     filter(strategy=="mice m=20")%>%
                     mutate(rho = 0.3, strategy="mice m=20 per arm"),
                   bind_rows(k5)%>%
                     filter(strategy=="mice m=10")%>%
                     mutate(rho = -0.3),
                   bind_rows(k6)%>%
                     filter(strategy=="mice m=10")%>%
                     mutate(rho = 0.3),
                   bind_rows(k7)%>%
                     filter(strategy=="mice m=5")%>%
                     mutate(rho = 0.3),
                   k8%>%
                     filter(strategy=="mice m=100")%>%
                     mutate(rho = -0.3),
                   bind_rows(k9)%>%
                     filter(strategy=="mice m=5")%>%
                     mutate(rho = -0.3),
                   bind_rows(k10)%>%
                     filter(strategy=="mice m=5")%>%
                     mutate(rho = 0.3, strategy="mice m=5 per arm"),
                   bind_rows(k11)%>%
                     filter(strategy=="mice m=5")%>%
                     mutate(rho = -0.3, strategy="mice m=5 per arm"))



t2.H0 <- t1.H0%>%
  mutate(bias = round((phat.d-M2)/M2,4))%>%
  dplyr::group_by(scenario.id, strategy, missing, do, rho)%>%
  dplyr::summarise(type1=mean(reject.h0), mean.bias = mean(bias))%>%
  dplyr::mutate(missing.new = case_when(missing=="mar1" ~ "0%",
                                        missing=="mar2" ~ "-5%",
                                        missing=="mar3" & do==0.1 ~ "-10%",
                                        missing=="mar3" & do==0.2 ~ "-15%",
                                        missing=="mar4" & do==0.1 ~  "5%",
                                        missing=="mar4" & do==0.2 ~ "-25%",
                                        missing=="mar5" & do==0.1 ~  "10%",
                                        missing=="mar5" & do==0.2 ~  "5%",
                                        missing=="mar6" ~ "15%",
                                        missing=="mar7" ~ "25%"))%>%
  dplyr::left_join(ss.bounds%>%
                     dplyr::select(scenario.id, p_C, M2, n.arm), by = "scenario.id")

t2.H0$missing.new <- factor(t2.H0$missing.new,levels=unique(t2.H0$missing.new)[c(4,3,2,1,5,6,7,8)])
t2.H0$flabel <- sprintf('p[C]: %s, Delta: %s, n:%s, rho:%s',t2.H0$p_C,t2.H0$M2,t2.H0$n.arm, t2.H0$rho)

mice.eval <- t2.H0%>%
  filter(strategy%in%c("cca", "mice m=20", "mice m=5 per arm","mice m=20 per arm","mice m=5", "mice m=100"), missing!="mcar")%>%
  ggplot(aes(y=missing.new,x=type1,colour=strategy)) + 
  geom_point() + 
  facet_wrap(~flabel,ncol=2) + 
  geom_vline(xintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  labs(x = "Empirical Type-I error",
       y = "Drop-out difference (%C-%T)" ,
       title = "Empirical Type-I error different MICE options") +
  theme_bw() +
  theme(legend.position = 'bottom')

mice.eval <- t2.H0%>%
  filter(strategy%in%c("cca", "mice m=20", "mice m=5 per arm","mice m=20 per arm","mice m=5", "mice m=100"), missing!="mcar")%>%
  ggplot(aes(y=missing.new,x=mean.bias,colour=strategy)) + 
  geom_point() + 
  facet_wrap(~flabel,ncol=2) + 
  geom_vline(xintercept=c(-.1,.1),
             linetype=2) + 
  labs(x = "Empirical Type-I error",
       y = "Drop-out difference (%C-%T)" ,
       title = "Empirical Type-I error different MICE options") +
  theme_bw() +
  theme(legend.position = 'bottom')


t2.H0%>%
  filter(strategy%in%c("cca","mice m=5"), missing=="mcar")%>%
  ggplot(aes(y=missing.new,x=type1,colour=strategy)) + 
  geom_point() + 
  facet_wrap(~flabel,ncol=2) + 
  geom_vline(xintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  labs(x = "Empirical Type-I error",
       y = "Drop-out difference (%C-%T)" ,
       title = "p.title") +
  theme_bw() +
  theme(legend.position = 'bottom')

t2.H0%>%
  filter(strategy%in%c("cca", "mice m=5", "mice m=20", "mice m=100"), missing!="mcar", rho==-0.3)%>%
  ggplot(aes(y=missing.new,x=type1,colour=strategy)) + 
  geom_point() + 
  facet_wrap(~flabel,ncol=2) + 
  geom_vline(xintercept=c(0.9,1.1)*0.025,
             linetype=2) + 
  labs(x = "Empirical Type-I error",
       y = "Drop-out difference (%C-%T)" ,
       title = "p.title") +
  theme_bw() +
  theme(legend.position = 'bottom')


t2.H0%>%
  filter(strategy%in%c("cca", "mice m=5", "mice m=20", "mice m=100"), missing!="mcar")%>%
  ggplot(aes(y=missing.new,x=mean.bias,colour=strategy)) + 
  geom_point() + 
  facet_wrap(~rho,ncol=2) + 
  geom_vline(xintercept=c(-0.1,0.1),
             linetype=2) + 
  labs(x = "Bias",
       y = "Drop-out difference (%C-%T)" ,
       title = "p.title") +
  theme_bw() +
  theme(legend.position = 'bottom')
