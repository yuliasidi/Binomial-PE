library(dplyr)
library(purrr)

scenarios <- expand.grid(
  TYPE=c('wald'),
  #TYPE=c('wald','waldxm30','waldxmm','waldxp80'),
  MISSING=c('mcar','mar1','mar2','mar3','mar4','mar5','mar6','mnar1','mnar2'),
  PERCENT=sprintf('%02d',seq(5,25,5))
  ,stringsAsFactors = FALSE)%>%
#  mutate(i = seq(1,18000,1))
  mutate(i = seq(1,45,1))

scenarios <- scenarios%>%
  dplyr::mutate(REPN1 = 100*(REPN-1)+1,
                REPN2 = 100*REPN)

scenarios_list <- as.list(scenarios)

purrr::pwalk(.l = scenarios_list,
             .f = function(TYPE,MISSING,PERCENT,REPN,REPN1,REPN2,i)
             cat(
                  whisker::whisker.render(
                    readLines('Step3_5_mice/anal_mice_wald.tmpl'),
                    data = list(TYPE = TYPE,
                                MISSING = MISSING,
                                PERCENT = PERCENT,
                                REPN = REPN,
                                REPN1 = REPN1,
                                REPN2 = REPN2,
                                i = i)
                    ),
                  file = file.path('Step3_5_mice/wald',
                                   sprintf('step_3_5_type_%s_missing_%s_percent_%s_%d.R',
                                           TYPE,MISSING,PERCENT,REPN)
                                   ),
                  sep='\n')
             )
