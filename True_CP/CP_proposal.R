
source("Step_0_init.R")
source("True_CP/CP_functions.R")


bineval.025 <- data.frame(expand.grid(n1 = seq(90,2100,10), p1 = seq(0.65,0.95,0.1)))%>%
  mutate(n2 = n1,
         p2= p1 - 0.025)

bineval.10 <- data.frame(expand.grid(n1 = seq(90,2100,10), p1 = seq(0.65,0.95,0.1)))%>%
  mutate(n2 = n1,
         p2= p1 - 0.1)

bineval.20 <- data.frame(expand.grid(n1 = seq(90,2100,10), p1 = seq(0.65,0.95,0.1)))%>%
  mutate(n2 = n1,
         p2= p1 - 0.2)


bineval.025.cp.wald <- bind_cols(bineval.025,cp = pmap_dbl(bineval.025,cp_2prop.wald, alpha=0.025))
bineval.10.cp.wald  <- bind_cols(bineval.10,cp = pmap_dbl(bineval.10,cp_2prop.wald, alpha=0.025))
bineval.20.cp.wald  <- bind_cols(bineval.20,cp = pmap_dbl(bineval.20,cp_2prop.wald, alpha=0.025))

saveRDS(bineval.025.cp.wald, 'True_CP/summary/bineval.025.cp.wald.rds')
saveRDS(bineval.10.cp.wald, 'True_CP/summary/bineval.10.cp.wald.rds')
saveRDS(bineval.20.cp.wald, 'True_CP/summary/bineval.20.cp.wald.rds')

bineval.025.cp.wilson <- bind_cols(bineval.025,cp = pmap_dbl(bineval.025,cp_2prop.wilson, alpha=0.025))
bineval.10.cp.wilson  <- bind_cols(bineval.10,cp = pmap_dbl(bineval.10,cp_2prop.wilson, alpha=0.025))
bineval.20.cp.wilson  <- bind_cols(bineval.20,cp = pmap_dbl(bineval.20,cp_2prop.wilson, alpha=0.025))

saveRDS(bineval.025.cp.wilson, 'True_CP/summary/bineval.025.cp.wilson.rds')
saveRDS(bineval.10.cp.wilson, 'True_CP/summary/bineval.10.cp.wilson.rds')
saveRDS(bineval.20.cp.wilson, 'True_CP/summary/bineval.20.cp.wilson.rds')



bineval.025.small <- data.frame(expand.grid(n1 = seq(1,90,1), p1 = seq(0.65,0.95,0.1)))%>%
  mutate(n2 = n1,
         p2= p1 - 0.025)

bineval.10.small <- data.frame(expand.grid(n1 = seq(1,90,1), p1 = seq(0.65,0.95,0.1)))%>%
  mutate(n2 = n1,
         p2= p1 - 0.1)

bineval.20.small <- data.frame(expand.grid(n1 = seq(1,90,1), p1 = seq(0.65,0.95,0.1)))%>%
  mutate(n2 = n1,
         p2= p1 - 0.2)


bineval.025.cp.wald.small <-
  bind_cols(bineval.025.small,cp = pmap_dbl(bineval.025.small,cp_2prop.wald, alpha=0.025))
bineval.10.cp.wald.small <- 
  bind_cols(bineval.10.small,cp = pmap_dbl(bineval.10.small,cp_2prop.wald, alpha=0.025))
bineval.20.cp.wald.small  <- 
  bind_cols(bineval.20.small,cp = pmap_dbl(bineval.20.small,cp_2prop.wald, alpha=0.025))

saveRDS(bineval.025.cp.wald.small, 'True_CP/summary/bineval.025.cp.wald.small.rds')
saveRDS(bineval.10.cp.wald.small, 'True_CP/summary/bineval.10.cp.wald.small.rds')
saveRDS(bineval.20.cp.wald.small, 'True_CP/summary/bineval.20.cp.wald.small.rds')








bin.025 <- readRDS("True_CP/bineval.025.rds")

bin.025.p <- bin.025%>%
  filter(n%in%c(152,354,556,758))%>%
  select(-n)
  

n1<-1600
n2<-n1
p1<-0.65
p2<-0.625
M2<-0.025
dt.bin <- data.frame(expand.grid(x = seq(0,n1,1),
                                 y = seq(0,n2,1)))
dt.bin <- dt.bin%>%
  mutate(x.pmf = dbinom(x,n1,p1),
         y.pmf = dbinom(y,n2,p2),
         pmf   = x.pmf * y.pmf,
         a = 2, 
         b = -1*(2 + x/n1 + y/n2 + M2*3), 
         c = M2^2 + M2*(2*x/n1+2) + x/n1 + y/n2,
         d = -x/n1*M2*(1+M2))%>%
  mutate(
    v = b^3/(27*a^3)-b*c/(6*a^2)+d/(2*a),
    sv = sign(v),
    #sv = ifelse(sv==0,1,sv),
    u = sv*(b^2/(9*a^2)-c/(3*a))^0.5,
    #u1 = ifelse(u==0,0.0001,u),
    w_val = v/u^3
  )%>%
  mutate(w_val = case_when(w_val  >= 1 ~ as.numeric(1),
                            w_val <= (-1) ~ as.numeric(-1),
                           TRUE ~ as.numeric(w_val)))

dt.bin1 <- dt.bin%>%
  #dplyr::filter(is.na(w_flag)=="FALSE")%>%
  mutate(test=acos(w_val))
    w = 1/3*(pi+acos(w_val))
  )%>%
  mutate(p1.rmle = 2*u*cos(w)-b/(3*a),
         p2.rmle = p1.rmle-M2)




bineval.025.cp.p <- bind_cols(bin.025.p, cp = pmap_dbl(bin.025.p,
                                             cp_2prop.fm, alpha=0.025))
bineval.10.cp.fm <- bind_cols(bineval.10%>%
                                 mutate(M2=0.1),
                               cp = pmap_dbl(bineval.10%>%
                                               mutate(M2=0.1),
                                             cp_2prop.fm, alpha=0.025))
bineval.20.cp.fm <- bind_cols(bineval.20%>%
                                mutate(M2=0.2),
                              cp = pmap_dbl(bineval.20%>%
                                              mutate(M2=0.2),
                                            cp_2prop.fm, alpha=0.025))



saveRDS(bineval.025.cp.fm, 'True_CP/summary/bineval.025.cp.fm.rds')
saveRDS(bineval.10.cp.fm, 'True_CP/summary/bineval.10.cp.fm.rds')
saveRDS(bineval.20.cp.fm, 'True_CP/summary/bineval.20.cp.fm.rds')



#add smaller SS
bineval.025.sm <- data.frame(expand.grid(n1 = seq(10,89,1), p1 = seq(0.65,0.95,0.1)))%>%
  mutate(n2 = n1,
         p2= p1 - 0.025)

bineval.10.sm <- data.frame(expand.grid(n1 = seq(10,89,1), p1 = seq(0.65,0.95,0.1)))%>%
  mutate(n2 = n1,
         p2= p1 - 0.1)

bineval.20.sm <- data.frame(expand.grid(n1 = seq(1,89,10), p1 = seq(0.65,0.95,0.1)))%>%
  mutate(n2 = n1,
         p2= p1 - 0.2)

bineval.025.cp.wald.sm <- bind_cols(bineval.025.sm,cp = pmap_dbl(bineval.025.sm,cp_2prop.wald, alpha=0.025))
bineval.10.cp.wald.sm  <- bind_cols(bineval.10.sm,cp = pmap_dbl(bineval.10.sm,cp_2prop.wald, alpha=0.025))
bineval.20.cp.wald  <- bind_cols(bineval.20,cp = pmap_dbl(bineval.20,cp_2prop.wald, alpha=0.025))

bineval.10.cp.wilson.sm  <- bind_cols(bineval.10.sm,cp = pmap_dbl(bineval.10.sm,cp_2prop.wilson, alpha=0.025))


bineval.10.cp.wald.sm%>%
  ggplot2::ggplot(aes(x = n1, y = cp)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size=0.1) +
  ggplot2::geom_hline(yintercept=0.95, colour="red") +
  #ggplot2::scale_y_continuous(limits = c(0.94, 1)) +
  facet_wrap(~p1)


bineval.10.cp.wilson.sm%>%
  ggplot2::ggplot(aes(x = n1, y = cp)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size=0.1) +
  ggplot2::geom_hline(yintercept=0.95, colour="red") +
  #ggplot2::scale_y_continuous(limits = c(0.94, 1)) +
  facet_wrap(~p1)

#check large SS behavior margin 0.05
bineval <- data.frame(n1 = 1000, n2 = 1000, p1 = seq(0,1,0.005))%>%
  mutate(p2 = p1 - 0.05)%>%
  filter(p2>0)

bineval.cp <- bind_cols(bineval,cp = pmap_dbl(bineval,cp_2prop.wald, alpha=0.025))

bineval.cp%>%
  ggplot2::ggplot(aes(x = p1, y = cp)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size=0.1) +
  ggplot2::geom_hline(yintercept=0.95, colour="red")


#fig#2 in Zhang et al 2010
bineval <- data.frame(n1 = 100, n2 = 100, p1 = seq(0,1,0.01))%>%
  mutate(p2 = 0.5)

bineval.cp <- bind_cols(bineval,cp = pmap_dbl(bineval,cp_2prop.wilson, alpha=0.025))


bineval.cp%>%
  ggplot2::ggplot(aes(x = p1, y = cp)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size=0.1) +
  ggplot2::geom_hline(yintercept=0.95, colour="red") +
  ggplot2::scale_y_continuous(limits = c(0.85, 1)) +
  ggplot2::scale_x_continuous(breaks = seq(0,1,0.2))

#fig#2(lower) in Brown et al 2005
bineval <- data.frame(n1 = 13, n2 = 10, p1 = seq(0,1,0.005))%>%
  mutate(p2 = p1  )

bineval.cp <- bind_cols(bineval,cp = pmap_dbl(bineval,cp_2prop.wilson, alpha=0.025))

bineval.cp%>%
  ggplot2::ggplot(aes(x = p1, y = cp)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size=0.1) +
  ggplot2::geom_hline(yintercept=0.95, colour="red") +
  ggplot2::scale_x_continuous(breaks = seq(0,1,0.2))




