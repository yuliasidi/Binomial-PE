#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')


dt.full.X <- readRDS(file = sprintf('dtfullwald_%d.rds',idx))

set.seed(81762+idx+165)

#generate mar5 for 5-25% by 5% DO
dt.mar5 <- miss.apply.do(dt.full.X, b.trt=2, b.y=0, b.X=-2, do=0.25)

dt.mar5.check <- dt.miss.check(dt.mar5 ,0.25)


saveRDS(dt.mar5.check,file = sprintf('walddochmar525_%d.rds',idx))

saveRDS(dt.mar5,file = sprintf('dtwaldmar525_%d.rds',idx))

