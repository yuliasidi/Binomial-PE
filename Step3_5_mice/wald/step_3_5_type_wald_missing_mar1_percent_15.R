#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

##################
###   MICE    ####
##################

dt.miss <- readRDS(file = sprintf('dtwaldmar115_%d.rds',idx))

mice.anal <- mice.apply(dt.miss)%>%
  mutate(type = 'wald', missing = 'mar1', do = '15'  )

saveRDS(mice.anal,file = sprintf('micewaldmar115_%d.rds',idx))

