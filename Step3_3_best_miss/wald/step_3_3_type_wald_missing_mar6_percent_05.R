#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

################################
###   BEST CASE SCENARIO    ####
################################

dt.miss <- readRDS(file = sprintf('dtwaldmar605_%d.rds',idx))

best.anal <- best.wald.anal(dt.miss)%>%
  mutate(type = 'wald', missing = 'mar6', do = '05'  )

saveRDS(best.anal,file = sprintf('bestwaldmar605_%d.rds',idx))

