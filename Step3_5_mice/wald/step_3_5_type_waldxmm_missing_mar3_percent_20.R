#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

##################
###   MICE    ####
##################

dt.miss <- readRDS(file = sprintf('dtwaldxmmmar320_%d.rds',idx))

mice.anal <- mice.apply(dt.miss)%>%
  mutate(type = 'waldxmm', missing = 'mar3', do = '20'  )

saveRDS(mice.anal,file = sprintf('micewaldxmmmar320_%d.rds',idx))

