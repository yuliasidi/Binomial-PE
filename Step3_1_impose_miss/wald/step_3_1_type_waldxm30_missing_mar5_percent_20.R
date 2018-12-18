#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')


dt.full.X <- readRDS(file = sprintf('dtfullwaldxm30_%d.rds',idx))

set.seed(81762+idx+130)

#generate mar5 for 5-25% by 5% DO
dt.mar5 <- miss.apply.do(dt.full.X, b.trt=2, b.y=0, b.X=-2, do=0.20)

dt.mar5.check <- dt.miss.check(dt.mar5 ,0.20)


saveRDS(dt.mar5.check,file = sprintf('waldxm30dochmar520_%d.rds',idx))

saveRDS(dt.mar5,file = sprintf('dtwaldxm30mar520_%d.rds',idx))

