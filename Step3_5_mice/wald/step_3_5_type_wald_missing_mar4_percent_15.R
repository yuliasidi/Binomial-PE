#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

##################
###   MICE    ####
##################

dt.miss <- readRDS(file = sprintf('dtwaldmar415_%d.rds',idx))

mice.anal <- mice.apply(dt.miss)%>%
  mutate(type = 'wald', missing = 'mar4', do = '15'  )

saveRDS(mice.anal,file = sprintf('micewaldmar415_%d.rds',idx))

