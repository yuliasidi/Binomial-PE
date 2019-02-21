#define seeds for each dataset used in this sumulation study

df.names <- expand.grid(
  MISSING=c('mcar','mar1','mar2','mar3','mar4','mar5','mar6','mnar1','mnar2'),
  PERCENT=sprintf('%02d',seq(5,25,5)),
  TYPE=c('wald','waldxm30','waldxmm','waldxp80'),
  idx = seq(0,29,1))

df_names <- df.names%>%  
  dplyr::mutate(dtname = sprintf('dt%s%s%s_%s.rds',TYPE, MISSING, PERCENT, idx),
                seedadd = seq(1, length(dtname),1))


saveRDS(df_names,"df_names.rds")
