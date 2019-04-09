plot.power <- function(df, do.val, p.title){
  df%>%
    dplyr::filter(do==do.val&!is.na(missing.new))%>%
    ggplot(aes(y=missing.new,x=power,colour=strategy)) + 
    geom_point() + 
    facet_wrap(~flabel,ncol=2) + 
    geom_vline(xintercept=0.9,
               linetype=2) + 
    labs(x = "Empirical Power",
         y = "Drop-out difference (%C-%T)" ,
         title = p.title) +
    theme_bw() +
    theme(legend.position = 'bottom')
  
  
}
