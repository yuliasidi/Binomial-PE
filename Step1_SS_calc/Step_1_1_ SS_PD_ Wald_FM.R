source("Step_0_init.r")


# Fixed margin approach for proportions difference.
# The margin was set at 5%, 7.5%, 10%, 15%, 20%.
# The success rates for the primary endpoint range btw 0.6-0.95 by increments of 0.05.
# alpha level is 2.5%.
# Power is 0.9.

M2 <- c(0.05, 0.075, 0.1, 0.15, 0.2)
p_C <- seq(0.6, 0.95, 0.05)

#M2 <- c(0.05, 0.1, 0.15)
#p_C <- c(0.35)

#alpha <- c(0.025)
#beta <- c(0.1)

# Set all possible combinations for SS calculation
ass.for.ss <- as.data.frame(expand.grid(p_C, M2+0, alpha, 1-beta))%>%
  rename(p_C = Var1, M2 = Var2, alpha = Var3, power = Var4 )%>%
  mutate(p_T = p_C, q_C = 1-p_C, d=round(M2-q_C,2))%>%
  mutate(ex = ifelse(d>0,"Y","N"))%>% #Exlcude scenarios where the qc will be > X2
  mutate(M2.new = ifelse(d==0,M2/2,M2))%>%
  filter(ex == "N")%>%
  select(-d, -ex, -M2)%>%
  rename(M2 = M2.new)

ass.for.ss <- unique(ass.for.ss)%>%mutate(scenario.id = seq(1,n(),1))


#########################################################
########      Sample Size Calculations        ###########
#########################################################

# Sample Size is calculated under the assumption of p_T=p_C for H_1 as in Dann & Koch (2008)
# Same formula appears in Blackwelder 1982

SS <- ass.for.ss%>%
  mutate(N.total.Wald = 2*round((qnorm(1-alpha)+
                                   qnorm(power))^2*(p_C*(1-p_C)+p_T*(1-p_T))/(p_C-p_T-M2)^2,0))

# Sample size formula is taken from Farrington-Manning (1990)
SS <- SS%>%
  p.rmle.fm()%>%
  mutate(N.total.FM = 2*round((qnorm(1-alpha)*sqrt((p_C.rmle*(1-p_C.rmle)+p_T.rmle*(1-p_T.rmle)))+
                                 qnorm(power)*sqrt(p_C*(1-p_C)+p_T*(1-p_T)))^2/(p_C-p_T-M2)^2,0))%>%
  mutate(N.total.d = N.total.FM-N.total.Wald)

saveRDS(SS, file = "SS_Wald_FM_power90.rds")