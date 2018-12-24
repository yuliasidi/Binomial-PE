#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

##################
###   MICE    ####
##################

dt.miss <- readRDS(file = sprintf('dtwaldmar520_%d.rds',idx))

mice.anal <- mice.apply(dt.miss)%>%
  mutate(type = 'wald', missing = 'mar5', do = '20'  )

saveRDS(mice.anal,file = sprintf('micewaldmar520_%d.rds',idx))

