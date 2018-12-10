
library(dplyr)
library(purrr)
library(ggplot2)

source("True_CP/CP_functions.R")


######################
## One proportion   ##
######################


data.p1 <- data.frame(expand.grid(n = seq(100,1000,100), p = seq(0.6, 0.95, 0.05)))

data.p1.cp.wald <- bind_cols(data.p1,cp = pmap_dbl(data.p1,cp_1prop.wald, alpha=0.025))
data.p1.cp.wn <- bind_cols(data.p1,cp = pmap_dbl(data.p1,cp_1prop.wilson, alpha=0.025))

data.p1.cp <- bind_rows(data.p1.cp.wald%>%
                          mutate(method="Wald"),
                        data.p1.cp.wn%>%
                          mutate(method="Wilson"))

#Plot CP for different p1 and SS
cp.p1.plot <- data.p1.cp%>%
  ggplot2::ggplot(aes(x = p, y = cp, color = method)) +
  ggplot2::geom_line(aes(linetype = method)) +
  ggplot2::geom_point(size=0.1) +
  ggplot2::geom_hline(yintercept=0.95, colour="grey") +
  ggplot2::scale_y_continuous(breaks = seq(0.87,1, 0.01)) +
  facet_wrap(~n)

#Zoom in on p1=0.95
data.p1.95 <- data.frame(n = seq(100,1000,1), p = 0.95)

data.p1.95.cp.wald <- bind_cols(data.p1.95,cp = pmap_dbl(data.p1.95,cp_1prop.wald, alpha=0.025))
data.p1.95.cp.wn <- bind_cols(data.p1.95,cp = pmap_dbl(data.p1.95,cp_1prop.wilson, alpha=0.025))

data.p1.95.cp <- bind_rows(data.p1.95.cp.wald%>%
                          mutate(method="Wald"),
                        data.p1.95.cp.wn%>%
                          mutate(method="Wilson"))
cp.p1.95.plot <- data.p1.95.cp%>%
  ggplot2::ggplot(aes(x = n, y = cp, color = method)) +
  ggplot2::geom_line(aes(linetype = method)) +
  ggplot2::geom_point(size=0.1) +
  ggplot2::geom_hline(yintercept=0.95, colour="grey") +
  ggplot2::scale_y_continuous(breaks = seq(0.87,1, 0.01))


#Zoom in on p1=0.90
data.p1.90 <- data.frame(n = seq(100,1000,1), p = 0.90)

data.p1.90.cp.wald <- bind_cols(data.p1.90,cp = pmap_dbl(data.p1.90,cp_1prop.wald, alpha=0.025))
data.p1.90.cp.wn <- bind_cols(data.p1.90,cp = pmap_dbl(data.p1.90,cp_1prop.wilson, alpha=0.025))

data.p1.90.cp <- bind_rows(data.p1.90.cp.wald%>%
                             mutate(method="Wald"),
                           data.p1.90.cp.wn%>%
                             mutate(method="Wilson"))
cp.p1.90.plot <- data.p1.90.cp%>%
  ggplot2::ggplot(aes(x = n, y = cp, color = method)) +
  ggplot2::geom_line(aes(linetype = method)) +
  ggplot2::geom_point(size=0.1) +
  ggplot2::geom_hline(yintercept=0.95, colour="grey") +
  ggplot2::scale_y_continuous(breaks = seq(0.87,1, 0.01))

pdf("True_CP/CP_P1.pdf")
cp.p1.plot
dev.off()

pdf("True_CP/CP_P1_90.pdf")
cp.p1.90.plot
dev.off()

pdf("True_CP/CP_P1_95.pdf")
cp.p1.95.plot
dev.off()

############################
## Two proportions diff   ##
############################
plot.p2.d <- function(d){
  data.p2.d0 <- data.frame(expand.grid(n1 = seq(100,1000,100), p1 = seq(0.6, 0.95, 0.05)))%>%
    mutate(n2 = n1, p2 = p1 - d)
  
  data.p2.d0.cp.wald <- bind_cols(data.p2.d0,cp = pmap_dbl(data.p2.d0,cp_2prop.wald, alpha=0.025))
  data.p2.d0.cp.wn <- bind_cols(data.p2.d0,cp = pmap_dbl(data.p2.d0,cp_2prop.wilson, alpha=0.025))
  
  data.p2.d0.cp <- bind_rows(data.p2.d0.cp.wald%>%
                               mutate(method="Wald"),
                             data.p2.d0.cp.wn%>%
                               mutate(method="Wilson"))
  cp.p2.d0.plot <- data.p2.d0.cp%>%
    ggplot2::ggplot(aes(x = p1, y = cp, color = method)) +
    ggplot2::geom_line(aes(linetype = method)) +
    ggplot2::geom_point(size=0.1) +
    ggplot2::geom_hline(yintercept=0.95, colour="grey") +
    #ggplot2::scale_y_continuous(breaks = seq(0.87,1, 0.01)) +
    facet_wrap(~n1)
return(cp.p2.d0.plot)  
}


data.p2.d0 <- data.frame(expand.grid(n1 = seq(100,1000,1), p1 = 0.95))%>%
  mutate(n2 = n1, p2 = p1 - 0.2)

data.p2.d0.cp.wald <- bind_cols(data.p2.d0,cp = pmap_dbl(data.p2.d0,cp_2prop.wald, alpha=0.025))
data.p2.d0.cp.wn <- bind_cols(data.p2.d0,cp = pmap_dbl(data.p2.d0,cp_2prop.wilson, alpha=0.025))

data.p2.d0.cp <- bind_rows(data.p2.d0.cp.wald%>%
                             mutate(method="Wald"),
                           data.p2.d0.cp.wn%>%
                             mutate(method="Wilson"))
cp.p2.d0.plot <- data.p2.d0.cp%>%
  ggplot2::ggplot(aes(x = n1, y = cp, color = method)) +
  ggplot2::geom_line(aes(linetype = method)) +
  ggplot2::geom_point(size=0.1) +
  ggplot2::geom_hline(yintercept=0.95, colour="grey") 
  #ggplot2::scale_y_continuous(breaks = seq(0.87,1, 0.01)) +
  

cp.p2.d0.plot <- plot.p2.d(d=0)

pdf("True_CP/CP_P2_d0.pdf")
cp.p2.d0.plot
dev.off()

cp.p2.d05.plot <- plot.p2.d(d=0.05)

pdf("True_CP/CP_P2_d05.pdf")
cp.p2.d05.plot
dev.off()


cp.p2.d10.plot <- plot.p2.d(d=0.10)

pdf("True_CP/CP_P2_d10.pdf")
cp.p2.d10.plot
dev.off()


cp.p2.d15.plot <- plot.p2.d(d=0.15)

pdf("True_CP/CP_P2_d15.pdf")
cp.p2.d15.plot
dev.off()

cp.p2.d20.plot <- plot.p2.d(d=0.20)

pdf("True_CP/CP_P2_d20.pdf")
cp.p2.d20.plot
dev.off()
