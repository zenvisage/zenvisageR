process <- function(argtype, inputaxisvar, count, ggplot1, ggplot2) {
  library(pdist)
  #d = a$data["State"]
  #c = a$data$State
  xAttribute <- ggplot1$labels$x
  yAttribute <- ggplot1$labels$y
  zAttribute <- ggplot1$labels$colour
  
  x1 <- ggplot1$data[[xAttribute]]
  y1 <- ggplot1$data[[yAttribute]]
  z1 <- ggplot1$data[[zAttribute]]
  # reshape y1 into rows 
  numVCs <- length(unique(z1)) # grabs how many levels of the factor are actually present in z2 
  y1 <- matrix(y1, nrows <- numVCs, byrow = TRUE) #byrow fills entries rowwise. Each row contains the observations per zattribute
  x2 <- ggplot2$data[[xAttribute]]
  y2 <- ggplot2$data[[yAttribute]]
  z2 <- ggplot2$data[[zAttribute]]
  # reshape y2 into columns
  numDataPoints <- length(z2) # z2 contains the zAttributes for the points
  numVCs <- length(unique(z2)) # grabs how many levels of the factor are actually present in z2
  rows <- numDataPoints / numVCs
  y2 <- matrix(y2, nrows <- rows) # each column contains the observations per zattribute
  
  out <- pdist(y1, t(y2))  # computes euclidean distance betwen rows of matrix X and rows of matrix Y and flattens
  maxIndex <- which.max(out@dist)
  # convert flattened index (starts from 1 of course in R) to the (pair) tested
  # 51 x 2 -> 102 x 1
  #row <- round((maxIndex - 1) / out@p) + 1
  #col <- (maxIndex - 1) %% out@p + 1
  N <- count
  
  #grabs the indices of what we want to output
  if (argtype == "argmax") {
    ndx <- order(out@dist, decreasing = T)[1:N]
  } else {
    ndx <- order(out@dist)[1:N]
  }

  outrows <- round((ndx - 1) / out@p) + 1 # elementwise
  outcols <- (ndx - 1) %% out@p + 1
  
  axis_vars <- list(unique(inputaxisvar)[outrows])
  #lapply converts from Factor to corresponding list
  # example: given a factor v1 for the states, v1[1] is AK, because AK is the first level of the factor
  # v1[2] is MD, because MD is the 21 level of the factor. lapply basically converts from this factor format to character format
  outaxisvar = unlist(lapply(axis_vars, as.character))
  outaxisvar <- factor(outaxisvar, levels=unique(as.character(outaxisvar)) )
  print(ndx)
  print(outaxisvar)
  print(out@dist[ndx])
  return(outaxisvar)
}
