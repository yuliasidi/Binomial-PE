#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])+1
.libPaths('ysidi/lib')

dt.mcar <- saveRDS(file = sprintf('dtmcarwald_%02d.rds',idx))

#calculate type1 and power
dt.mcar <- dt.mcar%>%
  mutate(Wald.H0.m5  = pmap(list(df=t.H0.m5,  M2 =  M2), Wald.CI.rm, y=y.m),
         Wald.H0.m10 = pmap(list(df=t.H0.m10, M2 =  M2), Wald.CI.rm, y=y.m),
         Wald.H0.m15 = pmap(list(df=t.H0.m15, M2 =  M2), Wald.CI.rm, y=y.m),
         Wald.H0.m20 = pmap(list(df=t.H0.m20, M2 =  M2), Wald.CI.rm, y=y.m),
         Wald.H0.m25 = pmap(list(df=t.H0.m25, M2 =  M2), Wald.CI.rm, y=y.m),
         Wald.H1.m5  = pmap(list(df=t.H1.m5,  M2 =  M2), Wald.CI.rm, y=y.m),
         Wald.H1.m10 = pmap(list(df=t.H1.m10, M2 =  M2), Wald.CI.rm, y=y.m),
         Wald.H1.m15 = pmap(list(df=t.H1.m15, M2 =  M2), Wald.CI.rm, y=y.m),
         Wald.H1.m20 = pmap(list(df=t.H1.m20, M2 =  M2), Wald.CI.rm, y=y.m),
         Wald.H1.m25 = pmap(list(df=t.H1.m25, M2 =  M2), Wald.CI.rm, y=y.m))

dt.mcar <- dt.mcar%>%
  mutate(type1.Wald.m5 = map(Wald.H0.m5, reject.H0),
         power.Wald.m5 = map(Wald.H1.m5, reject.H0),
         type1.Wald.m10 = map(Wald.H0.m10, reject.H0),
         power.Wald.m10 = map(Wald.H1.m10, reject.H0),
         type1.Wald.m15 = map(Wald.H0.m15, reject.H0),
         power.Wald.m15 = map(Wald.H1.m15, reject.H0),
         type1.Wald.m20 = map(Wald.H0.m20, reject.H0),
         power.Wald.m20 = map(Wald.H1.m20, reject.H0),
         type1.Wald.m25 = map(Wald.H0.m25, reject.H0),
         power.Wald.m25 = map(Wald.H1.m25, reject.H0))

dt.mcar.sum <- dt.mcar%>%
  select(scenario.id, p_T, M2, 
         type1.Wald.m5, power.Wald.m5, type1.Wald.m10, power.Wald.m10,
         type1.Wald.m15, power.Wald.m15, type1.Wald.m20, power.Wald.m20,
         type1.Wald.m25, power.Wald.m25)%>%
  mutate(type1.Wald.m5 = map_dbl(type1.Wald.m5, as.numeric),
         power.Wald.m5 = map_dbl(power.Wald.m5, as.numeric),
         type1.Wald.m10 = map_dbl(type1.Wald.m10, as.numeric),
         power.Wald.m10 = map_dbl(power.Wald.m10, as.numeric),
         type1.Wald.m15 = map_dbl(type1.Wald.m15, as.numeric),
         power.Wald.m15 = map_dbl(power.Wald.m15, as.numeric),
         type1.Wald.m20 = map_dbl(type1.Wald.m20, as.numeric),
         power.Wald.m20 = map_dbl(power.Wald.m20, as.numeric),
         type1.Wald.m25 = map_dbl(type1.Wald.m25, as.numeric),
         power.Wald.m25 = map_dbl(power.Wald.m25, as.numeric))

saveRDS(dt.mcar.sum,file = sprintf('mcarsumwald_%02d.rds',idx))
