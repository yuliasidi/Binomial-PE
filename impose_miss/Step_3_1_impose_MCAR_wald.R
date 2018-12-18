#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

dt.full.Xp30 <- readRDS(file = sprintf('dtfullwald_%d.rds',idx))
dt.full.Xp80 <- readRDS(file = sprintf('dtfullwaldxp80_%d.rds',idx))
dt.full.Xm30 <- readRDS(file = sprintf('dtfullwaldxm30_%d.rds',idx))
dt.full.Xm80 <- readRDS(file = sprintf('dtfullwaldxmm_%d.rds',idx))

set.seed(31682+idx)

######################################
#    generate MCAR for X RHO=0.3     #
######################################
dt.Xp30.mcar5  <- miss.apply.do(dt.full.Xp30, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.05)
dt.Xp30.mcar10 <- miss.apply.do(dt.full.Xp30, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.10)
dt.Xp30.mcar15 <- miss.apply.do(dt.full.Xp30, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.15)
dt.Xp30.mcar20 <- miss.apply.do(dt.full.Xp30, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.20)
dt.Xp30.mcar25 <- miss.apply.do(dt.full.Xp30, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.25)

#check DO rates
dt.miss.check(dt.Xp30.mcar5 ,0.05)
dt.miss.check(dt.Xp30.mcar10,0.10)
dt.miss.check(dt.Xp30.mcar15,0.15)
dt.miss.check(dt.Xp30.mcar20,0.20)
dt.miss.check(dt.Xp30.mcar25,0.25)

#save dt with MCAR
saveRDS(dt.Xp30.mcar5,file = sprintf('dtmcar5waldxp30_%d.rds',idx))
saveRDS(dt.Xp30.mcar10,file = sprintf('dtmcar10waldxp30_%d.rds',idx))
saveRDS(dt.Xp30.mcar15,file = sprintf('dtmcar15waldxp30_%d.rds',idx))
saveRDS(dt.Xp30.mcar20,file = sprintf('dtmcar20waldxp30_%d.rds',idx))
saveRDS(dt.Xp30.mcar25,file = sprintf('dtmcar25waldxp30_%d.rds',idx))



######################################
#    generate MCAR for X RHO=0.8     #
######################################
dt.Xp80.mcar5  <- miss.apply.do(dt.full.Xp80, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.05)
dt.Xp80.mcar10 <- miss.apply.do(dt.full.Xp80, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.10)
dt.Xp80.mcar15 <- miss.apply.do(dt.full.Xp80, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.15)
dt.Xp80.mcar20 <- miss.apply.do(dt.full.Xp80, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.20)
dt.Xp80.mcar25 <- miss.apply.do(dt.full.Xp80, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.25)

#check DO rates
dt.miss.check(dt.Xp80.mcar5 ,0.05)
dt.miss.check(dt.Xp80.mcar10,0.10)
dt.miss.check(dt.Xp80.mcar15,0.15)
dt.miss.check(dt.Xp80.mcar20,0.20)
dt.miss.check(dt.Xp80.mcar25,0.25)

#save dt with MCAR
saveRDS(dt.Xp80.mcar5,file  = sprintf('dtmcar5waldxpp_%d.rds',idx))
saveRDS(dt.Xp80.mcar10,file = sprintf('dtmcar10waldxpp_%d.rds',idx))
saveRDS(dt.Xp80.mcar15,file = sprintf('dtmcar15waldxpp_%d.rds',idx))
saveRDS(dt.Xp80.mcar20,file = sprintf('dtmcar20waldxpp_%d.rds',idx))
saveRDS(dt.Xp80.mcar25,file = sprintf('dtmcar25waldxpp_%d.rds',idx))


######################################
#    generate MCAR for X RHO=-0.3    #
######################################
dt.Xm30.mcar5  <- miss.apply.do(dt.full.Xm30, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.05)
dt.Xm30.mcar10 <- miss.apply.do(dt.full.Xm30, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.10)
dt.Xm30.mcar15 <- miss.apply.do(dt.full.Xm30, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.15)
dt.Xm30.mcar20 <- miss.apply.do(dt.full.Xm30, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.20)
dt.Xm30.mcar25 <- miss.apply.do(dt.full.Xm30, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.25)

#check DO rates
dt.miss.check(dt.Xm30.mcar5 ,0.05)
dt.miss.check(dt.Xm30.mcar10,0.10)
dt.miss.check(dt.Xm30.mcar15,0.15)
dt.miss.check(dt.Xm30.mcar20,0.20)
dt.miss.check(dt.Xm30.mcar25,0.25)

#save dt with MCAR
saveRDS(dt.Xm30.mcar5,file  = sprintf('dtmcar5waldxm30_%d.rds',idx))
saveRDS(dt.Xm30.mcar10,file = sprintf('dtmcar10waldxm30_%d.rds',idx))
saveRDS(dt.Xm30.mcar15,file = sprintf('dtmcar15waldxm30_%d.rds',idx))
saveRDS(dt.Xm30.mcar20,file = sprintf('dtmcar20waldxm30_%d.rds',idx))
saveRDS(dt.Xm30.mcar25,file = sprintf('dtmcar25waldxm30_%d.rds',idx))



######################################
#    generate MCAR for X RHO=-0.8    #
######################################
dt.Xm80.mcar5  <- miss.apply.do(dt.full.Xm80, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.05)
dt.Xm80.mcar10 <- miss.apply.do(dt.full.Xm80, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.10)
dt.Xm80.mcar15 <- miss.apply.do(dt.full.Xm80, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.15)
dt.Xm80.mcar20 <- miss.apply.do(dt.full.Xm80, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.20)
dt.Xm80.mcar25 <- miss.apply.do(dt.full.Xm80, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.25)

#check DO rates
dt.miss.check(dt.Xm80.mcar5 ,0.05)
dt.miss.check(dt.Xm80.mcar10,0.10)
dt.miss.check(dt.Xm80.mcar15,0.15)
dt.miss.check(dt.Xm80.mcar20,0.20)
dt.miss.check(dt.Xm80.mcar25,0.25)

#save dt with MCAR
saveRDS(dt.Xm80.mcar5,file  = sprintf('dtmcar5waldxmm_%d.rds',idx))
saveRDS(dt.Xm80.mcar10,file = sprintf('dtmcar10waldxmm_%d.rds',idx))
saveRDS(dt.Xm80.mcar15,file = sprintf('dtmcar15waldxmm_%d.rds',idx))
saveRDS(dt.Xm80.mcar20,file = sprintf('dtmcar20waldxmm_%d.rds',idx))
saveRDS(dt.Xm80.mcar25,file = sprintf('dtmcar25waldxmm_%d.rds',idx))
