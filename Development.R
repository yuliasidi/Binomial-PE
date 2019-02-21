
t <- dt.full.X$t.Wald.H1.X[[1]]




t1<-mnar.fun(t)

t1%>%filter(sim.id==1)%>%summarise(p.full = mean(y, na.rm = T),
                                                 p.mnar = mean(y.mnar.do10, na.rm = T))
