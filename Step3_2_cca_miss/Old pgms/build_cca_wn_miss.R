scenarios <- expand.grid(
  TYPE=c('wn'),
  MISSING=c('mcar','mar', 'mnar'),
  PERCENT=sprintf('%02d',seq(5,25,5))
  ,stringsAsFactors = FALSE)

scenarios_list <- as.list(scenarios)

purrr::pwalk(.l = scenarios_list,
             .f = function(TYPE,MISSING,PERCENT)
             cat(
                  whisker::whisker.render(
                    readLines('cca_miss/cca_wn_miss.tmpl'),
                    data = list(TYPE = TYPE,
                                MISSING = MISSING,
                                PERCENT = PERCENT)
                    ),
                  file = file.path('cca_miss',
                                   sprintf('step_3_2_type_%s_missing_%s_percent_%s.R',TYPE,MISSING,PERCENT)
                                   ),
                  sep='\n')
             )
