missing.desc.adj <- function(df, do.adj = 15){
  if (do.adj==15){
    df%>%
      dplyr::mutate(missing.desc = case_when(missing=="mar4" ~ "  5%",
                                             missing=="mar5" ~ " 10%",
                                             TRUE ~ as.character(missing.desc)))  
  }
  
}
