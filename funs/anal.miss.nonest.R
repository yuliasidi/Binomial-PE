
#model that generates missing data and analyzes it by CCA/best/worse or nested MI strategy
anal.miss.nonest <- function(df, M2, b.trt = 0, b.Y = 0, b.X1 = 0, b.X2 = 0, b.ty = 0, do = 0.1, seed = seed,
                          ci.method = FM.CI,
                          seed.mice = seed.mice)
{
  
  out<-miss.impose.x2.cont(df = df, b.trt = b.trt, b.Y = b.Y, b.X1 = b.X1, b.X2 = b.X2, b.ty = b.ty, do = do, seed = seed)
  
    #mice
      out.ci <- out%>%
        mice.run.perarm(n.mi = num.n.mi.nonest, ci.method=ci.method, M2=M2, seed.mice = seed.mice)%>%
        dplyr::mutate(strategy = sprintf("mice n = %s", num.n.mi.nonest))

    anal.return <- list(out.ci)%>%purrr::set_names(c("ci"))
    
    return(anal.return)
    
  }    
  



