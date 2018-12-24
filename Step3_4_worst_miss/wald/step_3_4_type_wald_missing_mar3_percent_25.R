#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

################################
###   WORST CASE SCENARIO    ####
################################

dt.miss <- readRDS(file = sprintf('dtwaldmar325_%d.rds',idx))

worst.anal <- worst.wald.anal(dt.miss)%>%
  mutate(type = 'wald', missing = 'mar3', do = '25'  )

saveRDS(worst.anal,file = sprintf('worstwaldmar325_%d.rds',idx))

