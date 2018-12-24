#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

################################
###   BEST CASE SCENARIO    ####
################################

dt.miss <- readRDS(file = sprintf('dtwaldxp80mar310_%d.rds',idx))

best.anal <- best.wald.anal(dt.miss)%>%
  mutate(type = 'waldxp80', missing = 'mar3', do = '10'  )

saveRDS(best.anal,file = sprintf('bestwaldxp80mar310_%d.rds',idx))

