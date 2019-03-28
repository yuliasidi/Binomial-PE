plot.type1 <- function(df, miss.type, p.title){
  
  df%>%
    dplyr::filter(missing==miss.type)%>%
    ggplot(aes(x = do, y = type1)) +
    geom_point(aes(color = strategy)) +
    geom_hline(yintercept=c(0.9,1.1)*0.025,
               linetype=2) +
    #  scale_y_continuous(breaks = c(seq(0,0.05, 0.025), 0.075, seq(0.1, 1, 0.1))) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       breaks = c(0, 0.10, 0.20)) +
    #  facet_wrap(~f,labeller = label_parsed) +
    facet_wrap(~sc) +
    labs(x = "Drop-out rates", y = "Empirical type-I error", 
         title = p.title) +
    theme_bw() +
    theme(legend.position = 'bottom')
  
}
