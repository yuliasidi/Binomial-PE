p.rmle.fm <- function(df, M2=M2){
  df%>%
    mutate(a = 2, 
           b = -1*(2 + p_C + p_T + M2*3), 
           c = M2^2 + M2*(2*p_C+2) + p_C + p_T,
           d = -p_C*M2*(1+M2))%>%
    mutate(v = b^3/(27*a^3)-b*c/(6*a^2)+d/(2*a),
           u = sign(v)*(b^2/(9*a^2)-c/(3*a))^0.5,
           w = 1/3*(pi+acos(v/u^3)))%>%
    mutate(p_C.rmle = 2*u*cos(w)-b/(3*a),
           p_T.rmle = p_C.rmle-M2)%>%
    select(-a, -b, -c, -d, -v, -u, -w)
}
