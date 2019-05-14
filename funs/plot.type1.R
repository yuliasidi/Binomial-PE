plot.type1 <- function(df, do.val, p.title){
  df%>%
    dplyr::filter(do==do.val)%>%
    ggplot(aes(y=missing.desc,x=type1,colour=strategy)) + 
    geom_point() + 
    facet_wrap(~flabel,ncol=2) + 
    geom_vline(xintercept=c(0.9,1.1)*0.025,
               linetype=2) + 
    labs(x = "Empirical Type-I error",
         y = "Drop-out difference (%C-%T)" ,
         title = p.title) +
    theme_bw() +
    theme(legend.position = 'bottom')
  
  
}
