

tmp<-as.data.frame(check.mech.mcar$mech.H0.m20)
ggplot(tmp, aes(x=rp, y=rm))+
  geom_point()+
  facet_wrap(~sim.id)


tmp<-as.data.frame(check.mech.mar$mech.H0.m20)
ggplot(tmp, aes(x=rp, y=rm))+
  geom_point()+
  facet_wrap(~sim.id)

tmp<-as.data.frame(check.mech.mar$mech.H0.m15)
ggplot(tmp, aes(x=rp, y=rm))+
  geom_point()+
  facet_wrap(~sim.id)

tmp<-as.data.frame(check.mech.mnar$mech.H0.m20)
ggplot(tmp, aes(x=rp, y=rm))+
  geom_point()+
  facet_wrap(~sim.id)

