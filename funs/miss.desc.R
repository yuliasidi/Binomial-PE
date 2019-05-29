miss.desc <- function(df){
  


    df%>%
      dplyr::mutate(missing.desc = case_when(missing=="mar1" ~ "  0%",
                                             missing=="mar2" ~ " -5%",
                                             missing=="mar3" ~  "-10%",
                                             missing=="mar4" ~  "-15%",
                                             missing=="mar5" ~  "  5%",
                                             missing=="mar6" ~  " 10%",
                                             missing=="mar7" ~  " 15%",
                                             missing=="mcar" ~  "mcar",
                                             missing=="mnar1"~  "p_T_obs > p_T_full",
                                             missing=="mnar2"~  "p_C_obs < p_C_full"))  
    

  
}
