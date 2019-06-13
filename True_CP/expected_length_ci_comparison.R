library(dplyr)
library(purrr)

wald.el <- map_df(list.files("True_CP/summary/Expected_length", "wald", full.names = T ), readRDS)

wn.el <- map_df(list.files("True_CP/summary/Expected_length", "wilson", full.names = T ), readRDS)

el.comp <- 
  left_join(wald.el%>%
            dplyr::rename(el.wald = el),
          wn.el%>%
            dplyr::rename(el.wn = el), by = c( "n1", "p1", "n2", "p2"))%>%
  dplyr::mutate(diff= el.wald - el.wn)
