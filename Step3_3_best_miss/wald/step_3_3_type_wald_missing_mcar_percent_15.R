#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

################################
###   BEST CASE SCENARIO    ####
################################

dt.miss <- readRDS(file = sprintf('dtwaldmcar15_%d.rds',idx))

best.anal <- best.wald.anal(dt.miss)%>%
  mutate(type = 'wald', missing = 'mcar', do = '15'  )

saveRDS(best.anal,file = sprintf('bestwaldmcar15_%d.rds',idx))

