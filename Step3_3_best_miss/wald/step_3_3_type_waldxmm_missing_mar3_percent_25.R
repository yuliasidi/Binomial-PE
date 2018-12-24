#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

################################
###   BEST CASE SCENARIO    ####
################################

dt.miss <- readRDS(file = sprintf('dtwaldxmmmar325_%d.rds',idx))

best.anal <- best.wald.anal(dt.miss)%>%
  mutate(type = 'waldxmm', missing = 'mar3', do = '25'  )

saveRDS(best.anal,file = sprintf('bestwaldxmmmar325_%d.rds',idx))

