#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

################################
###   BEST CASE SCENARIO    ####
################################

dt.miss <- readRDS(file = sprintf('dtwaldmnar215_%d.rds',idx))

best.anal <- best.wald.anal(dt.miss)%>%
  mutate(type = 'wald', missing = 'mnar2', do = '15'  )

saveRDS(best.anal,file = sprintf('bestwaldmnar215_%d.rds',idx))

