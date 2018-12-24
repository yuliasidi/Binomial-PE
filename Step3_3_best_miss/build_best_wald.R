library(dplyr)
library(purrr)

scenarios <- expand.grid(
  TYPE=c('wald','waldxm30','waldxmm','waldxp80'),
  MISSING=c('mcar','mar1','mar2','mar3','mar4','mar5','mar6','mnar1','mnar2'),
  PERCENT=sprintf('%02d',seq(5,25,5))
  ,stringsAsFactors = FALSE)%>%
  mutate(i = seq(1,180,1))

scenarios_list <- as.list(scenarios)

purrr::pwalk(.l = scenarios_list,
             .f = function(TYPE,MISSING,PERCENT,i)
             cat(
                  whisker::whisker.render(
                    readLines('Step3_3_best_miss/anal_best_wald.tmpl'),
                    data = list(TYPE = TYPE,
                                MISSING = MISSING,
                                PERCENT = PERCENT,
                                i = i)
                    ),
                  file = file.path('Step3_3_best_miss/wald',
                                   sprintf('step_3_3_type_%s_missing_%s_percent_%s.R',TYPE,MISSING,PERCENT)
                                   ),
                  sep='\n')
             )
