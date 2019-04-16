miss.param.assign.x2.cont <- function(do = 0.2){


  if(do == 0.2){
   
    m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4", "mar5", "mar6", "mar7"),
                      bt = c(0, 0, 0.3, 0.6, .9, -0.3, -0.6, -.9),
                      bx2 = c(0, 2, 2, 2, 2, 2, 2, 2),
                      bx1 = c(0, 0, 0, 0, 0, 0, 0, 0),
                      by = c(0, 0, 0, 0, 0, 0, 0, 0)) 
  }
    
  if(do == 0.1){
  
    m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4", "mar5"),
                      b.trt = c(0, 0, 0.04, 0.09, -0.04, -0.09),
                      b.X = c(0, 1.5, 1.5, 1.5, 1.5, 1.5),
                      b.Y = c(0, 0, 0, 0, 0, 0))
    
  }
  

  return(m.param)  
}
