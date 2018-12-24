#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

#################
###   CCA    ####
#################

dt.miss <- readRDS(file = sprintf('dtwaldxm30mar225_%d.rds',idx))

cca.anal <- cca.wald.anal(dt.miss)%>%
  mutate(type = 'waldxm30', missing = 'mar2', do = '25'  )

saveRDS(cca.anal,file = sprintf('ccawaldxm30mar225_%d.rds',idx))

