
source('Step_0_init.R')

SS <- readRDS(file = "SS_power90Wald.rds")

set.seed(854763)

SS <- SS%>%
  as_tibble()%>%
  mutate(N_T = 100000, 
         N_C = 100000,
         p_T0 = p_C - M2,
         t.H0 = pmap(list(N_T,N_C,p_T0,p_C),ni.d),
         t.H1 = pmap(list(N_T,N_C,p_T,p_C),ni.d))%>%
  mutate(t.H0.X = map(t.H0, .f = function(df){
    df.T <- df%>%
      filter(trt=='T')%>%
      mutate(X = rnorm(length(trt),4,1))%>%
      mutate(X = case_when(X<0 ~ 0, 
                           X>10 ~ 10, 
                           TRUE ~ as.numeric(X)))
    df.C <- df%>%
      filter(trt=='C')%>%
      mutate(X = rnorm(length(trt),4,1))%>%
      mutate(X = case_when(X<0 ~ 0, 
                           X>10 ~ 10, 
                           TRUE ~ as.numeric(X)))
    df.TC <- bind_rows(df.T, df.C)
    return(df.TC)}),
    t.H1.X = map(t.H1, .f = function(df){
      df.T <- df%>%
        filter(trt=='T')%>%
        mutate(X = rnorm(length(trt),4,1))%>%
        mutate(X = case_when(X<0 ~ 0, 
                             X>10 ~ 10, 
                             TRUE ~ as.numeric(X)))
      df.C <- df%>%
        filter(trt=='C')%>%
        mutate(X = rnorm(length(trt),4,1))%>%
        mutate(X = case_when(X<0 ~ 0, 
                             X>10 ~ 10, 
                             TRUE ~ as.numeric(X)))
      df.TC <- bind_rows(df.T, df.C)
      return(df.TC)}))

#check that the simulated p's match the target ones
SSch <- SS%>%
  group_by(scenario.id)%>%
  mutate(t.H0.pch = map(t.H0, .f = function(df){
    df%>%
      group_by(trt)%>%
      summarise(ybar = mean(y))
  }),
  t.H1.pch = map(t.H1, .f = function(df){
    df%>%
      group_by(trt)%>%
      summarise(ybar = mean(y))
  }))%>%
  select(scenario.id, p_C, p_T0, p_T, M2, t.H0.pch, t.H1.pch)

#calculate lower/upper bound for spearman correlation coef.
bounds <- SS%>%
  mutate(rho.bound.H0.l = map(t.H0.X, bound.l),
         rho.bound.H1.l = map(t.H1.X, bound.l),
         rho.bound.H0.u = map(t.H0.X, bound.u),
         rho.bound.H1.u = map(t.H1.X, bound.u),
         rho.bound.H0.l = map_dbl(rho.bound.H0.l, as.numeric),
         rho.bound.H1.l = map_dbl(rho.bound.H1.l, as.numeric),
         rho.bound.H0.u = map_dbl(rho.bound.H0.u, as.numeric),
         rho.bound.H1.u = map_dbl(rho.bound.H1.u, as.numeric))%>%
  select(scenario.id, p_C, p_T0, p_T, M2,
         rho.bound.H0.l, rho.bound.H1.l, rho.bound.H0.u, rho.bound.H1.u)

saveRDS(bounds,'bounds.rds')
