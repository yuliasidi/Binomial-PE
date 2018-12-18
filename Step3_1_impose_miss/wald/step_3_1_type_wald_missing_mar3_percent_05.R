#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')


dt.full.X <- readRDS(file = sprintf('dtfullwald_%d.rds',idx))

set.seed(81762+idx+13)

#generate mar3 for 5-25% by 5% DO
dt.mar3 <- miss.apply.do(dt.full.X, b.trt=0, b.y=0, b.X=-2, do=0.05)

dt.mar3.check <- dt.miss.check(dt.mar3 ,0.05)


saveRDS(dt.mar3.check,file = sprintf('walddochmar305_%d.rds',idx))

saveRDS(dt.mar3,file = sprintf('dtwaldmar305_%d.rds',idx))

