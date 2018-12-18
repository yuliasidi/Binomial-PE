#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')


dt.full.X <- readRDS(file = sprintf('dtfullwald_%d.rds',idx))

set.seed(81762+idx+145)

#generate mcar for 5-25% by 5% DO
dt.mcar <- miss.apply.do(dt.full.X, b.trt=0, b.y=0, b.X=0, do=0.25)

dt.mcar.check <- dt.miss.check(dt.mcar ,0.25)


saveRDS(dt.mcar.check,file = sprintf('walddochmcar25_%d.rds',idx))

saveRDS(dt.mcar,file = sprintf('dtwaldmcar25_%d.rds',idx))

