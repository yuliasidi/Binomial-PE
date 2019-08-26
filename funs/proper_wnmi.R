proper_wnmi <- function(dt){
wn_mi_pc <- 
  dt%>%
  dplyr::mutate(C_phat_var = C_phat*(1 - C_phat)/C_n)%>%
  mi_comb(level=2, phat = 'C_phat', var_phat = 'C_phat_var')%>%
  dplyr::mutate(method = "wn-mi for pc",
                lower_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), lb_wn_ign, n_obs = dt$C_n[1]),
                upper_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), ub_wn_ign, n_obs = dt$C_n[1]))

wn_mi_pt <- 
  dt%>%
  dplyr::mutate(T_phat_var = T_phat*(1 - T_phat)/T_n)%>%
  mi_comb(level=2, phat = 'T_phat', var_phat = 'T_phat_var')%>%
  dplyr::mutate(method = "wn-mi for pt",
                lower_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), lb_wn_ign, n_obs = dt$T_n[1]),
                upper_bound = pmap_dbl(list(z = qt(0.975, df = v), qhat = qbar, rn = rn), ub_wn_ign, n_obs = dt$T_n[1]))

wn_mi <- 
  tibble(method = "wn-mi", 
         pc = wn_mi_pc$qbar, lb_pc = wn_mi_pc$lower_bound, ub_pc = wn_mi_pc$upper_bound,
         pt = wn_mi_pt$qbar, lb_pt = wn_mi_pt$lower_bound, ub_pt = wn_mi_pt$upper_bound,
         qbar.d = wn_mi_pc$qbar - wn_mi_pt$qbar)%>%
  dplyr::mutate(ci.l = pmap_dbl(list(pc, lb_pc, ub_pc, pt, lb_pt, ub_pt), lb_wn_p2),
                ci.u = pmap_dbl(list(pc, lb_pc, ub_pc, pt, lb_pt, ub_pt), ub_wn_p2))%>%
  dplyr::select(method, qbar.d, ci.l, ci.u)

}
