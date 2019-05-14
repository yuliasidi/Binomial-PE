plot.bias <- function(df, do.val, p.title){
  df%>%
    dplyr::filter(do==do.val)%>%
    ggplot(aes(y=missing.desc,x=mean.bias,colour=strategy)) + 
    geom_point() + 
    facet_wrap(~flabel,ncol=2) + 
    geom_vline(xintercept=c(-0.1,0.1),
               linetype=2) + 
    labs(x = "Mean relative bias",
         y = "Drop-out difference (%C-%T)" ,
         title = p.title) +
    theme_bw() +
    theme(legend.position = 'bottom')
  
  
}
