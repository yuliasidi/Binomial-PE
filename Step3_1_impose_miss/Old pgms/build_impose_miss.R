scenarios <- expand.grid(
  TYPE=c('wald','fm','wn'),
  MISSING=c('mcar','mar', 'mnar'),
  PERCENT=sprintf('%02d',seq(5,25,5))
  ,stringsAsFactors = FALSE)%>%
  dplyr::mutate(
    bTRT = dplyr::case_when(
      MISSING=='mcar' ~ 'log(1)',
      MISSING=='mar'  ~ 'log(1)',
      MISSING=='mnar' ~ 'log(1)'
      ),
    bY = dplyr::case_when(
      MISSING=='mcar' ~ 'log(1)',
      MISSING=='mar'  ~ 'log(1)',
      MISSING=='mnar' ~ 'log(10)'
    ),
    bX = dplyr::case_when(
      MISSING=='mcar' ~ 'log(1)',
      MISSING=='mar'  ~ 'log(10)',
      MISSING=='mnar' ~ 'log(10)'
    )
    )

scenarios_list <- as.list(scenarios)

purrr::pwalk(.l = scenarios_list,
             .f = function(TYPE,MISSING,PERCENT,bTRT,bY,bX)
             cat(
                  whisker::whisker.render(
                    readLines('impose_miss/impose_miss.tmpl'),
                    data = list(TYPE = TYPE,
                                MISSING = MISSING,
                                PERCENT = PERCENT,
                                bTRT = bTRT,
                                bY = bY,
                                bX = bX)
                    ),
                  file = file.path('impose_miss',
                                   sprintf('step_3_1_type_%s_missing_%s_percent_%s.R',TYPE,MISSING,PERCENT)
                                   ),
                  sep='\n')
             )

# Try alternative model parameters for missingness:

scenarios <- expand.grid(
  TYPE=c('wald'),
  MISSING=c('mar', 'mnar'),
  PERCENT=sprintf('%02d',seq(5,25,5))
  ,stringsAsFactors = FALSE)%>%
  dplyr::mutate(
    bTRT = dplyr::case_when(
      MISSING=='mar'  ~ 'log(1)',
      MISSING=='mnar' ~ 'log(1)'
    ),
    bY = dplyr::case_when(
      MISSING=='mar'  ~ 'log(1)',
      MISSING=='mnar' ~ '-log(10)'
    ),
    bX = dplyr::case_when(
      MISSING=='mar'  ~ '-log(10)',
      MISSING=='mnar' ~ '-log(10)'
    )
  )

scenarios_list <- as.list(scenarios)

purrr::pwalk(.l = scenarios_list,
             .f = function(TYPE,MISSING,PERCENT,bTRT,bY,bX)
               cat(
                 whisker::whisker.render(
                   readLines('impose_miss/impose_miss.tmpl'),
                   data = list(TYPE = TYPE,
                               MISSING = MISSING,
                               PERCENT = PERCENT,
                               bTRT = bTRT,
                               bY = bY,
                               bX = bX)
                 ),
                 file = file.path('impose_miss',
                                  sprintf('step_3_1_type_%s_missing_%s_percent_%snew.R',TYPE,MISSING,PERCENT)
                 ),
                 sep='\n')
)

