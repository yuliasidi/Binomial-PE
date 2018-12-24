#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

##################
###   MICE    ####
##################

dt.miss <- readRDS(file = sprintf('dtwaldxmmmcar25_%d.rds',idx))

mice.anal <- mice.apply(dt.miss)%>%
  mutate(type = 'waldxmm', missing = 'mcar', do = '25'  )

saveRDS(mice.anal,file = sprintf('micewaldxmmmcar25_%d.rds',idx))

