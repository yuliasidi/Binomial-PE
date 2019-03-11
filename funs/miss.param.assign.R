miss.param.assign <- function(df){
  # MCAR: .trt=0, b.y=0, b.X=0
  b.trt0 <- 0
  b.X0 <- 0
  
  # MAR1: b.trt=0, b.y=0, b.X=1
  b.trt1 <- 0
  b.X1 <- 1
  
  # MAR2: b.trt=0, b.y=0, b.X=-1
  b.trt2 <- 0
  b.X2 <- -1
  
  # MAR3: b.trt=2, b.y=0, b.X=1
  b.trt3 <- 0.5
  b.X3 <- 1
  
  # MAR4: b.trt=-0.5, b.y=0, b.X=-1
  b.trt4 <- -0.5
  b.X4 <- -1
  
  # MAR5: b.trt=0.5, b.y=0, b.X=-1
  b.trt5 <- 0.5
  b.X5 <- -1
  
  # MAR6: b.trt=-0.5, b.y=0, b.X=1
  b.trt6 <- -0.5
  b.X6 <- 1
  
  # MNAR1: b.trt=-.5, b.y=0, b.X=1
  b.trt7 <- 0
  b.X7 <- 1
  b.Y7 <- 0.5
  
  # MNAR2: b.trt=-0.5, b.y=0, b.X=1
  b.trt8 <- 0
  b.X8 <-  1
  b.Y8 <- -0.5
  
  df%>%
    mutate(b.trt = case_when(missing=='mcar'  ~ as.numeric(b.trt0),
                             missing=='mar1'  ~ as.numeric(b.trt1),
                             missing=='mar2'  ~ as.numeric(b.trt2),
                             missing=='mar3'  ~ as.numeric(b.trt3),
                             missing=='mar4'  ~ as.numeric(b.trt4),
                             missing=='mar5'  ~ as.numeric(b.trt5),
                             missing=='mar6'  ~ as.numeric(b.trt6),
                             missing=='mnar1' ~ as.numeric(b.trt7),
                             missing=='mnar2' ~ as.numeric(b.trt8)),
           b.X = case_when(missing=='mcar'  ~ as.numeric(b.X0),
                           missing=='mar1'  ~ as.numeric(b.X1),
                           missing=='mar2'  ~ as.numeric(b.X2),
                           missing=='mar3'  ~ as.numeric(b.X3),
                           missing=='mar4'  ~ as.numeric(b.X4),
                           missing=='mar5'  ~ as.numeric(b.X5),
                           missing=='mar6'  ~ as.numeric(b.X6),
                           missing=='mnar1' ~ as.numeric(b.X7),
                           missing=='mnar2' ~ as.numeric(b.X8)),
           b.Y = case_when(missing=='mnar1' ~ as.numeric(b.Y7),
                           missing=='mnar2' ~ as.numeric(b.Y8),
                           TRUE ~ as.numeric(0)))
}
