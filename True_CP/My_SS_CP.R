source("Step_0_init.R")
source("True_CP/CP_functions.R")


SS.wald <- readRDS("SS_power90Wald.rds")

SS.wald <- SS.wald%>%
  mutate(n1 = N.total/2,
         n2 = N.total/2,
         p2 = p_T-M2)%>%
  rename(p1 = p_C)%>%
  select(n1,n2,p1,p2)

SS.wald.cp <- bind_cols(SS.wald,cp = pmap_dbl(SS.wald,cp_2prop.wald, alpha=0.025))
