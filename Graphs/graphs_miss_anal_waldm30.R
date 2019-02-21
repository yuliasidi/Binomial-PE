source('Step_0_init.R')

cca_waldm30   <- readRDS("DataSummaries/cca_waldm30.rds")
best_waldm30  <- readRDS("DataSummaries/best_waldm30.rds")
worst_waldm30 <- readRDS("DataSummaries/worst_waldm30.rds")

#####################
####  CCA-MCAR  ####
#####################

cca_waldm30.mcar <-miss.param.assign(cca_waldm30)%>%
  filter(grepl('mcar',missing))

type1.cca_waldm30.mcar <- plot.type1.scenario(cca_waldm30.mcar, c(0, 0.05))
power.cca_waldm30.mcar <- plot.power.scenario(cca_waldm30.mcar, c(0.7, 0.92))
bias.cca_waldm30.mcar  <- plot.bias.scenario(cca_waldm30.mcar, c(-0.05, 0.05))


pdf("final_outputs/WaldM30/type1_cca_waldm30_mcar.pdf")
type1.cca_waldm30.mcar
dev.off()

pdf("final_outputs/WaldM30/power_cca_waldm30_mcar.pdf")
power.cca_waldm30.mcar
dev.off()

pdf("final_outputs/WaldM30/bias_cca_waldm30_mcar.pdf")
bias.cca_waldm30.mcar
dev.off()

#####################
####  CCA-MAR   ####
#####################

cca_waldm30.mar <-miss.param.assign(cca_waldm30)%>%
  filter(grepl('mar',missing))

type1.cca_waldm30.mar <- plot.type1.scenario(cca_waldm30.mar, c(0, 0.07))
power.cca_waldm30.mar <- plot.power.scenario(cca_waldm30.mar, c(0.65, 0.92))
bias.cca_waldm30.mar  <- plot.bias.scenario(cca_waldm30.mar, c(-0.16, 0.18))


pdf("final_outputs/WaldM30/type1_cca_waldm30_mar.pdf")
type1.cca_waldm30.mar
dev.off()

pdf("final_outputs/WaldM30/power_cca_waldM30_mar.pdf")
power.cca_waldm30.mar
dev.off()

pdf("final_outputs/WaldM30/bias_cca_waldm30_mar.pdf")
bias.cca_waldm30.mar
dev.off()

#####################
####  CCA-MNAR   ####
#####################

cca_waldm30.mnar <-miss.param.assign(cca_waldm30)%>%
  filter(grepl('mnar',missing))

type1.cca_waldm30.mnar <- plot.type1.scenario(cca_waldm30.mnar, c(0, 0.57), miss.type = 'mnar')
power.cca_waldm30.mnar <- plot.power.scenario(cca_waldm30.mnar, c(0.72, 1), miss.type = 'mnar')
bias.cca_waldm30.mnar  <-  plot.bias.scenario(cca_waldm30.mnar, c(-0.72, 0.22), miss.type = 'mnar')


pdf("final_outputs/WaldM30/type1_cca_waldm30_mnar.pdf")
type1.cca_waldm30.mnar
dev.off()

pdf("final_outputs/WaldM30/power_cca_waldm30_mnar.pdf")
power.cca_waldm30.mnar
dev.off()

pdf("final_outputs/WaldM30/bias_cca_waldm30_mnar.pdf")
bias.cca_waldm30.mnar
dev.off()


#####################
####  BEST-MCAR  ####
#####################

best_waldm30.mcar <-miss.param.assign(best_waldm30)%>%
  filter(grepl('mcar',missing))

type1.best_waldm30.mcar <- plot.type1.scenario(best_waldm30.mcar, c(0, 0.15))
power.best_waldm30.mcar <- plot.power.scenario(best_waldm30.mcar, c(0.89, 0.97))
bias.best_waldm30.mcar  <- plot.bias.scenario(best_waldm30.mcar, c(-0.26, 0))


pdf("final_outputs/WaldM30/type1_best_waldm30_mcar.pdf")
type1.best_waldm30.mcar
dev.off()

pdf("final_outputs/WaldM30/power_best_waldm30_mcar.pdf")
power.best_waldm30.mcar
dev.off()

pdf("final_outputs/WaldM30/bias_best_waldm30_mcar.pdf")
bias.best_waldm30.mcar
dev.off()

#####################
####  BEST-MAR   ####
#####################
best_waldm30.mar <-miss.param.assign(best_waldm30)%>%
  filter(grepl('mar',missing))

type1.best_waldm30.mar <- plot.type1.scenario(best_waldm30.mar, c(0, 1))
power.best_waldm30.mar <- plot.power.scenario(best_waldm30.mar, c(0, 1))
bias.best_waldm30.mar  <- plot.bias.scenario(best_waldm30.mar, c(-2.7, 2.2))


pdf("final_outputs/WaldM30/type1_best_waldm30_mar.pdf")
type1.best_waldm30.mar
dev.off()

pdf("final_outputs/WaldM30/power_best_waldm30_mar.pdf")
power.best_waldm30.mar
dev.off()

pdf("final_outputs/WaldM30/bias_best_waldm30_mar.pdf")
bias.best_waldm30.mar
dev.off()

#####################
####  BEST-MNAR   ####
#####################

best_waldm30.mnar <-miss.param.assign(best_waldm30)%>%
  filter(grepl('mnar',missing))

type1.best_waldm30.mnar <- plot.type1.scenario(best_waldm30.mnar, c(0.02, 1), miss.type = 'mnar')
power.best_waldm30.mnar <- plot.power.scenario(best_waldm30.mnar, c(0.89, 1), miss.type = 'mnar')
bias.best_waldm30.mnar  <- plot.bias.scenario(best_waldm30.mnar, c(-1.7, 0), miss.type = 'mnar')


pdf("final_outputs/WaldM30/type1_best_waldm30_mnar.pdf")
type1.best_waldm30.mnar
dev.off()

pdf("final_outputs/WaldM30/power_best_waldm30_mnar.pdf")
power.best_waldm30.mnar
dev.off()

pdf("final_outputs/WaldP30/bias_best_waldp30_mnar.pdf")
bias.best_waldp30.mnar
dev.off()

#####################
####  WORST-MCAR ####
#####################

worst_waldp30.mcar <-miss.param.assign(worst_waldp30)%>%
  filter(grepl('mcar',missing))

type1.worst_waldp30.mcar <- plot.type1.scenario(worst_waldp30.mcar, c(0, 0.15))
power.worst_waldp30.mcar <- plot.power.scenario(worst_waldp30.mcar, c(0.3, 0.91))
bias.worst_waldp30.mcar  <- plot.bias.scenario(worst_waldp30.mcar, c(-0.3, 0))


pdf("final_outputs/WaldP30/type1_worst_waldp30_mcar.pdf")
type1.worst_waldp30.mcar
dev.off()

pdf("final_outputs/WaldP30/power_worst_waldp30_mcar.pdf")
power.worst_waldp30.mcar
dev.off()

pdf("final_outputs/WaldP30/bias_worst_waldp30_mcar.pdf")
bias.worst_waldp30.mcar
dev.off()

#####################
####  WORST-MAR ####
#####################

worst_waldp30.mar <-miss.param.assign(worst_waldp30)%>%
  filter(grepl('mar',missing))

type1.worst_waldp30.mar <- plot.type1.scenario(worst_waldp30.mar, c(0, 1))
power.worst_waldp30.mar <- plot.power.scenario(worst_waldp30.mar, c(0, 1))
bias.worst_waldp30.mar  <- plot.bias.scenario(worst_waldp30.mar, c(-11, 11))


pdf("final_outputs/WaldP30/type1_worst_waldp30_mar.pdf")
type1.worst_waldp30.mar
dev.off()

pdf("final_outputs/WaldP30/power_worst_waldp30_mar.pdf")
power.worst_waldp30.mar
dev.off()

pdf("final_outputs/WaldP30/bias_worst_waldp30_mar.pdf")
bias.worst_waldp30.mar
dev.off()


#####################
####  WORST-MNAR ####
#####################

worst_waldp30.mnar <-miss.param.assign(worst_waldp30)%>%
  filter(grepl('mnar',missing))

type1.worst_waldp30.mnar <- plot.type1.scenario(worst_waldp30.mnar, c(0, 0.3), miss.type = 'mnar')
power.worst_waldp30.mnar <- plot.power.scenario(worst_waldp30.mnar, c(0.0, 0.91), miss.type = 'mnar')
bias.worst_waldp30.mnar  <- plot.bias.scenario(worst_waldp30.mnar, c(-.4, 5.5), miss.type = 'mnar')


pdf("final_outputs/WaldP30/type1_worst_waldp30_mnar.pdf")
type1.worst_waldp30.mnar
dev.off()

pdf("final_outputs/WaldP30/power_worst_waldp30_mnar.pdf")
power.worst_waldp30.mnar
dev.off()

pdf("final_outputs/WaldP30/bias_worst_waldp30_mnar.pdf")
bias.worst_waldp30.mnar
dev.off()

