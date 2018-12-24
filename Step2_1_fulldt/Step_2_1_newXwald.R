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

############################
#   add X with RHO=-0.3    #
############################

set.seed(87453162+idx)

dt.full.X <- add.X.rho(dt.full.noX, r =  -0.3) 

check.X <- bind_rows(dt.full.X$t.H0[[1]]%>%check_rho(),
                     dt.full.X$t.H1[[1]]%>%check_rho())

saveRDS(dt.full.X,file = sprintf('dtfullwaldxm30_%d.rds',idx))
saveRDS(check.X,file = sprintf('chdtfullwaldxm30_%d.rds',idx))


######################################
#   add X with RHO=lowest bound      #
######################################

set.seed(874531621+idx)

rho.lowest <- max(dt.full.noX$rho.bound.H0.l, dt.full.noX$rho.bound.H1.l)

  dt.full.X <- add.X.rho(dt.full.noX, r =  rho.lowest) 
  
  check.X <- bind_rows(dt.full.X$t.H0[[1]]%>%check_rho(),
                       dt.full.X$t.H1[[1]]%>%check_rho())
  
  saveRDS(dt.full.X,file = sprintf('dtfullwaldxmm_%d.rds',idx))
  saveRDS(check.X,file = sprintf('chdtfullwaldxpmm_%d.rds',idx))



######################################
#   add X with RHO=higest bound      #
######################################

set.seed(45316214+idx)

rho.highest <- min(dt.full.noX$rho.bound.H0.u, dt.full.noX$rho.bound.H1.u)


  dt.full.X <- add.X.rho(dt.full.noX, r =  rho.highest) 
  
  check.X <- bind_rows(dt.full.X$t.H0[[1]]%>%check_rho(),
                       dt.full.X$t.H1[[1]]%>%check_rho())
  
  saveRDS(dt.full.X,file = sprintf('dtfullwaldxp80_%d.rds',idx))
  saveRDS(check.X,file = sprintf('chdtfullwaldxpp_%d.rds',idx))

