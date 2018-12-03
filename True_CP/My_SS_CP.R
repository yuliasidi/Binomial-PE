source("Step_0_init.R")
source("True_CP/CP_functions.R")


SS.wald <- readRDS("SS_power90Wald.rds")

SS.wald <- SS.wald%>%
  mutate(n1 = N.total/2,
         n2 = N.total/2,
         p2 = p_T-M2)%>%
  rename(p1 = p_C)%>%
  select(scenario.id,n1,n2,p1,p2)

SS.wald.cp <- bind_cols(SS.wald,cp = pmap_dbl(SS.wald%>%
                                                select(-scenario.id),
                                              cp_2prop.wald, 
                                              alpha=0.025))

SS.fm <- readRDS("SS_power90FM.rds")

SS.fm <- SS.fm%>%
  mutate(n1 = N.total/2,
         n2 = N.total/2,
         p2 = p_T-M2)%>%
  rename(p1 = p_C)%>%
  select(scenario.id,n1,n2,p1,p2,M2)

SS.fm.cp <- bind_cols(SS.fm,cp = pmap_dbl(SS.fm%>%
                                            select(-scenario.id),
                                          cp_2prop.fm, 
                                          alpha=0.025))

SS.wn <- readRDS("SS_power90WN.rds")

SS.wn <- SS.wn%>%
  mutate(n1 = N.total/2,
         n2 = N.total/2,
         p2 = p_T-M2)%>%
  rename(p1 = p_C)%>%
  select(scenario.id,n1,n2,p1,p2)

SS.wn.cp <- bind_cols(SS.wn,cp = pmap_dbl(SS.wn%>%
                                            select(-scenario.id),
                                          cp_2prop.wilson, 
                                          alpha=0.025))

SS.cp <- bind_rows(SS.wald.cp%>%
                     select(scenario.id, cp)%>%
                     mutate(method = 'Wald'),
                   SS.fm.cp%>%
                     select(scenario.id, cp)%>%
                     mutate(method = 'FM'),
                   SS.wn.cp%>%
                     select(scenario.id, cp)%>%
                     mutate(method = 'WN'))

SS.cp.plot <- ggplot(SS.cp, aes(color = method, x = scenario.id, y = cp)) +
  geom_point() +
  scale_x_continuous(breaks = seq(1,30,5)) +
  scale_y_continuous(limits = c(0.945,0.955)) +
  xlab('Scenario ID') + 
  ylab("Coverage Probability")


pdf("True_CP/SS_cp.pdf")
SS.cp.plot
dev.off()