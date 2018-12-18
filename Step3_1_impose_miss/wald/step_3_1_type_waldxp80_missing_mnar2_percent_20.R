#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')


dt.full.X <- readRDS(file = sprintf('dtfullwaldxp80_%d.rds',idx))

set.seed(81762+idx+144)

#generate mnar2 for 5-25% by 5% DO
dt.mnar2 <- miss.apply.do(dt.full.X, b.trt=1, b.y=-2, b.X=-2, do=0.20)

dt.mnar2.check <- dt.miss.check(dt.mnar2 ,0.20)


saveRDS(dt.mnar2.check,file = sprintf('waldxp80dochmnar220_%d.rds',idx))

saveRDS(dt.mnar2,file = sprintf('dtwaldxp80mnar220_%d.rds',idx))

