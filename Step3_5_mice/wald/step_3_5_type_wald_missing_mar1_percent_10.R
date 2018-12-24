#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

##################
###   MICE    ####
##################

dt.miss <- readRDS(file = sprintf('dtwaldmar110_%d.rds',idx))

mice.anal <- mice.apply(dt.miss)%>%
  mutate(type = 'wald', missing = 'mar1', do = '10'  )

saveRDS(mice.anal,file = sprintf('micewaldmar110_%d.rds',idx))

