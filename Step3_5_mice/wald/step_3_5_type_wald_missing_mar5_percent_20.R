#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')

#################
###   MICE    ####
#################

f.nm <- sprintf('dtwaldmar520_%d.rds',idx)

dt.miss <- readRDS(file = f.nm)
df_names <- readRDS(file = "df_names.rds")

df_names <- readRDS(file = "df_names.rds")


seedadd <- df_names$seedadd[df_names$dtname==f.nm]

mice_sim <- rerun_mice(n = num.mi,
data = dt.miss,
f = mice.imp1,
seed = seedadd,
method = "logreg",
n.mi = 1)

saveRDS(mice_sim,file = sprintf('micewaldmar520_%d.rds',idx))
