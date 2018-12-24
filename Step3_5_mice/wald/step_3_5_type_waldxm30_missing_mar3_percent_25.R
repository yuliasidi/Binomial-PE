#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

##################
###   MICE    ####
##################

dt.miss <- readRDS(file = sprintf('dtwaldxm30mar325_%d.rds',idx))

mice.anal <- mice.apply(dt.miss)%>%
  mutate(type = 'waldxm30', missing = 'mar3', do = '25'  )

saveRDS(mice.anal,file = sprintf('micewaldxm30mar325_%d.rds',idx))

