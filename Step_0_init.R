#!/home/statsadmin/R/bin/Rscript



 # Initiations prior to running all the programs

library(tidyr, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(mice, warn.conflicts = F, quietly = T)

#source("PE_Bin_PD_Functions.R")

alpha <- c(0.025)
beta <- c(0.1)

# # of simulated studies
n.sim <- 10000

# # of imputations for MI
num.mi=10

