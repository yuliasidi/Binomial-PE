#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

#################
###   CCA    ####
#################

dt.miss <- readRDS(file = sprintf('dtwaldxp80mar305_%d.rds',idx))

cca.anal <- cca.wald.anal(dt.miss)%>%
  mutate(type = 'waldxp80', missing = 'mar3', do = '05'  )

saveRDS(cca.anal,file = sprintf('ccawaldxp80mar305_%d.rds',idx))

