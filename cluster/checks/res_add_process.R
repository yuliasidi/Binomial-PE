full.type1 <- map_df(list.files("cluster/out/overall", "full.type1", full.names = T), readRDS)

h0.sing <- map_df(list.files("cluster/out/overall", "h0.sing", full.names = T), readRDS)

#h0.sing$missing.desc <- factor(h0.sing$missing.desc,levels=unique(h0.sing$missing.desc)[c(4,3,2,1,5,6,7,8,9,10)])
h0.sing$missing.desc <- factor(h0.sing$missing.desc,levels=unique(h0.sing$missing.desc)[c(9,7,2,1,3,8,10,4,5,6)])

h0.sing <-
  h0.sing%>%
  left_join(ss%>%
              dplyr::filter(method=="wald")%>%
              dplyr::select(scenario.id, p_C, M2, n.arm), by = c("scenario.id"))%>%
  dplyr::mutate(flabel = sprintf('Delta:%s~p[C]:%s~n:%s', M2, p_C, n.arm))%>%
  left_join(full.type1%>%
              dplyr::select(scenario.id, method, reject.h0), 
            by = c("scenario.id", "method")) 


mcar.worst <- h0.sing%>%
  dplyr::filter(strategy=="worst", missing=="mcar", scenario.id == 8)%>%
  dplyr::arrange(type1)


mcar.best <- h0.sing%>%
  dplyr::filter(strategy=="best", missing=="mcar", scenario.id == 8)%>%
  dplyr::arrange(type1)




##################################

mnar1 <- h0.mnar%>%
  dplyr::filter(scenario.id%in%c(2,4,6,17,19,21,23,25,26))%>%
  dplyr::filter(missing.desc=="p_T_obs > p_T_full")
