process <- function(ggplot1, ggplot2) {
  #d = a$data["State"]
  #c = a$data$State
  xAttribute = ggplot1$labels$x
  yAttribute = ggplot1$labels$y
  zAttribute = ggplot1$labels$colour
  
  x1 = ggplot1$data[[xAttribute]]
  y1 = ggplot1$data[[yAttribute]]
  z1 = ggplot1$data[[zAttribute]]
  
  currz = z1[[1]]
  
  axis_vars <- list(z1[1], z1[3000])
  
  return(axis_vars)
}
