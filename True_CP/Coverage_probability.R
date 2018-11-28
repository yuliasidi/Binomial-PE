
library(dplyr)
library(purrr)
library(ggplot2)

source("True_CP/CP_functions.R")

bineval <- data.frame(n = c(17, 20, 25, 30, 35, 37, 42, 44, 49 ,
                            10, 12, 13, 15, 18, 23, 28, 33, 40))
bineval <- bineval%>%
  mutate(p = 0.5)

bineval.cp <- bind_cols(bineval,cp = pmap_dbl(bineval,cp_1prop.wald, alpha=0.025))

#check fig4 fro Agresti & Caffo 
bineval <- data.frame(n1 = 20, n2 = 20, p1 = seq(0,1,0.005))
bineval <- bineval%>%
  mutate(p2 = 0.1)

bineval.cp <- bind_cols(bineval,cp = pmap_dbl(bineval,cp_2prop.wald, alpha=0.025))

bineval.cp%>%
  ggplot2::ggplot(aes(x = p1, y = cp)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size=0.1) +
  ggplot2::geom_hline(yintercept=0.95, colour="red")

#fig#1(upper) in Brown et al 2005
bineval <- data.frame(n1 = 20, n2 = 20, p1 = seq(0,1,0.005), p2 = seq(0,1,0.005))

bineval.cp <- bind_cols(bineval,cp = pmap_dbl(bineval,cp_2prop.wald, alpha=0.025))

bineval.cp%>%
  ggplot2::ggplot(aes(x = p1, y = cp)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size=0.1) +
  ggplot2::geom_hline(yintercept=0.95, colour="red")

#fig#1(lower) in Brown et al 2005
bineval <- data.frame(n1 = 20, n2 = 20, p1 = seq(0,1,0.005))%>%
  mutate(p2 = p1 + 0.2 )%>%
  filter(p2 <= 1)

bineval.cp <- bind_cols(bineval,cp = pmap_dbl(bineval,cp_2prop.wald, alpha=0.025))

bineval.cp%>%
  ggplot2::ggplot(aes(x = p1, y = cp)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size=0.1) +
  ggplot2::geom_hline(yintercept=0.95, colour="red")


#fig#1 in Zhang et al 2010
bineval <- data.frame(n1 = 100, n2 = 100, p1 = seq(0,1,0.01))%>%
  mutate(p2 = 0.5 )

bineval.cp <- bind_cols(bineval,cp = pmap_dbl(bineval,cp_2prop.wald, alpha=0.025))

bineval.cp%>%
  ggplot2::ggplot(aes(x = p1, y = cp)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size=0.1) +
  ggplot2::geom_hline(yintercept=0.95, colour="red") +
  ggplot2::scale_y_continuous(limits = c(0.85, 1))


#check large SS behavior margin 0.05
bineval <- data.frame(n1 = 1000, n2 = 1000, p1 = seq(0,1,0.005))%>%
  mutate(p2 = p1 - 0.05)%>%
  filter(p2>0)

bineval.cp <- bind_cols(bineval,cp = pmap_dbl(bineval,cp_2prop.wald, alpha=0.025))

bineval.cp%>%
  ggplot2::ggplot(aes(x = p1, y = cp)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size=0.1) +
  ggplot2::geom_hline(yintercept=0.95, colour="red")


