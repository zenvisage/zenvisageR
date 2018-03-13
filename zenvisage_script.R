library(ggplot2)
housing <- read.csv("dataSets/landdata-states.csv")
a <- ggplot(housing, aes(x=Date, y=Home.Value, color=State)) + geom_line()
b <- ggplot(subset(housing, State %in% list("MA", "TX")), aes(x=Date, y=Home.Value, color=State)) + geom_line()
windows(xpos= 0, ypos = 0)
plot(a)
#print(a$data$State)
#plot(a)
#

v1 <- process(a,b)
#multiplot(a,b, cols=2)
#lapply converts from Factor to corresponding list
# eg v1[1] is AK, because AK is the first level of the factor
# v1[2] is MD, because MD is the 21 level of the factor. lapply basically converts from this factor format to character format
c <-ggplot(subset(housing, State %in% lapply(v1, as.character)), aes(x=Date, y=Home.Value, color=State)) + geom_line()
windows(xpos = -1, ypos = 0)
plot(c + facet_wrap(~State))

