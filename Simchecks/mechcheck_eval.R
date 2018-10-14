source('Step_0_init.R')

mech <- readRDS("simchecks/mechcheck.rds")

names(mech)

#if MCAR then rp should be equal to rm
mech.mcar <- mech%>%
  dplyr::filter(MISSING=="mcar")

mech.mar <- mech%>%
  dplyr::filter(MISSING=="mar")

mech.mnar <- mech%>%
  dplyr::filter(MISSING=="mnar")

x <- list(mech.mar,mech.mnar,mech.mcar)%>%
  purrr::map_df(.f=function(x){
    x%>%
      dplyr::select(scenario.id,PERCENT,mech.H0)%>%
      tidyr::unnest()  
  },.id='method')%>%
  dplyr::mutate(Method = case_when(method==1 ~ 'MAR',
                                         method==2 ~ 'MNAR',
                                         method==3 ~ 'MCAR'))

mech.check1 <- x%>%
  dplyr::filter(method!=3)%>%
  ggplot2::ggplot(ggplot2::aes(x=rp,y=rm,colour=Method)) +
  ggplot2::geom_point(alpha=0.1) +
  ggplot2::geom_smooth(se=FALSE,method='lm') +
  ggplot2::facet_wrap(~PERCENT) +
  ggplot2::ggtitle("MAR and MNAR % missing vs probability to be missing") +
  ggplot2::labs(x = "Assinged probability to be missing", y = "% of missing")

mech.check2 <- x%>%
  dplyr::filter(method==3)%>%
  ggplot2::ggplot(ggplot2::aes(x=rp,y=rm,colour=Method)) +
  ggplot2::geom_point(alpha=0.1) +
  ggplot2::geom_smooth(se=FALSE,method='lm') +
  ggplot2::facet_wrap(~PERCENT) +
  ggplot2::ggtitle("MCAR % missing vs probability to be missing") +
  ggplot2::labs(x = "Assinged probability to be missing", y = "% of missing")

pdf("Simchecks/mech_check1.pdf")
mech.check1
dev.off()

pdf("Simchecks/mech_check2.pdf")
mech.check2
dev.off()
