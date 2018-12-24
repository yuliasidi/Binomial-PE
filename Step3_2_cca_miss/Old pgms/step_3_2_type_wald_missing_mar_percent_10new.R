#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

dt.miss <- readRDS(file = sprintf('dtwaldnewmar10_%d.rds',idx))

#calculate type1 and power
dt.miss <- dt.miss%>%
  dplyr::mutate(t.H0.m1 = map(t.H0.m, .f=function(df){
    df%>%
      dplyr::rename(y=y.m)
  }),
  t.H1.m1 = map(t.H1.m, .f=function(df){
    df%>%
      dplyr::rename(y=y.m)
  }))%>%
  dplyr::select(-t.H0.m,-t.H1.m)%>%
  dplyr::rename(t.H0.m = t.H0.m1, t.H1.m = t.H1.m1)

dt.miss <- dt.miss%>%
  dplyr::mutate(t.H0.CI = pmap(list(df=t.H0.m, M2 =  M2), Wald.CI),
                t.H1.CI = pmap(list(df=t.H1.m, M2 =  M2), Wald.CI),
                t.H0.bias = pmap(list(t.H0.m, M2 = M2), bias.fun))
        
dt.miss <- dt.miss%>%
  mutate(type1 = map(t.H0.CI, reject.H0),
         power = map(t.H1.CI, reject.H0))

dt.miss.sum <- dt.miss%>%
  select(scenario.id, p_T, M2, type1, power, t.H0.bias)%>%
  mutate(type1 = map_dbl(type1, as.numeric),
         power = map_dbl(power, as.numeric),
         bias  = map_dbl(t.H0.bias, as.numeric))%>%
  dplyr::select(-t.H0.bias)


saveRDS(dt.miss.sum,file = sprintf('ccamarwaldnew10_%d.rds',idx))
