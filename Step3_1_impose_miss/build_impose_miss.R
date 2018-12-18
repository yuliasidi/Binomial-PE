library(dplyr)
library(purrr)

scenarios <- expand.grid(
  TYPE=c('wald','waldxm30','waldxmm','waldxp80'),
  MISSING=c('mcar','mar1','mar2','mar3','mar4','mar5','mar6','mnar1','mnar2'),
  PERCENT=sprintf('%02d',seq(5,25,5))
  ,stringsAsFactors = FALSE)%>%
  dplyr::mutate(
    bTRT = dplyr::case_when(
      MISSING=='mcar'  ~ '0',
      MISSING=='mar1'  ~ '0',
      MISSING=='mar2'  ~ '2',
      MISSING=='mar3'  ~ '0',
      MISSING=='mar4'  ~ '-2',
      MISSING=='mar5'  ~ '2',
      MISSING=='mar6'  ~ '-2',
      MISSING=='mnar1' ~ '0',
      MISSING=='mnar2' ~ '1'
      ),
    bY = dplyr::case_when(
      MISSING=='mnar1' ~ '2',
      MISSING=='mnar2' ~ '-2',
      TRUE ~ '0'
    ),
    bX = dplyr::case_when(
      MISSING=='mcar'  ~ '0',
      MISSING=='mar1'  ~ '2',
      MISSING=='mar2'  ~ '2',
      MISSING=='mar3'  ~ '-2',
      MISSING=='mar4'  ~ '-2',
      MISSING=='mar5'  ~ '-2',
      MISSING=='mar6'  ~ '2',
      MISSING=='mnar1' ~ '2',
      MISSING=='mnar2' ~ '-2'
    )
    )%>%
  mutate(i = seq(1,180,1))

scenarios_list <- as.list(scenarios)

purrr::pwalk(.l = scenarios_list,
             .f = function(TYPE,MISSING,PERCENT,bTRT,bY,bX,i)
             cat(
                  whisker::whisker.render(
                    readLines('Step3_1_impose_miss/impose_miss.tmpl'),
                    data = list(TYPE = TYPE,
                                MISSING = MISSING,
                                PERCENT = PERCENT,
                                bTRT = bTRT,
                                bY = bY,
                                bX = bX,
                                i = i)
                    ),
                  file = file.path('Step3_1_impose_miss/wald',
                                   sprintf('step_3_1_type_%s_missing_%s_percent_%s.R',TYPE,MISSING,PERCENT)
                                   ),
                  sep='\n')
             )
