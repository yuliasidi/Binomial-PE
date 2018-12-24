#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

dt.full <- readRDS(file = sprintf('dtfullwald_%d.rds',idx))
#dt.full <- readRDS(file = 'testfull_01.rds')

sc.id <- as.numeric(dt.full$scenario.id)

bounds <- readRDS(file = "bounds.rds")
bounds <- bounds%>%
  filter(scenario.id==sc.id)

#add bound values & remove previously generated X's
dt.full.noX <- bind_cols(dt.full, bounds%>%
                       select(rho.bound.H0.l, rho.bound.H1.l, rho.bound.H0.u, rho.bound.H1.u))%>%
  mutate(t.H0.X = map(t.H0, .f = function(df){
    df%>%
      select(-X)
  }),
  t.H1.X = map(t.H1, .f = function(df){
    df%>%
      select(-X)
  }))%>%
  select(-t.H0, -t.H1)%>%
  rename(t.H0 = t.H0.X, 
         t.H1 = t.H1.X)

#add X with specified rho value and then check
set.seed(8745862+idx)

dt.full.X <- add.X.rho(dt.full.noX, r =  0.15) 

check.X <- bind_rows(dt.full.X$t.H0[[1]]%>%check_rho(),
                     dt.full.X$t.H1[[1]]%>%check_rho())

saveRDS(dt.full.X,file = sprintf('dtfullwaldxp15_%d.rds',idx))
saveRDS(check.X,file = sprintf('chdtfullwaldxp15_%d.rds',idx))
