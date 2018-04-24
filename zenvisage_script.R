library(ggplot2)
housing <- read.csv("dataSets/landdata-states.csv")
f1 <- ggplot(housing, aes(x=Date, y=Home.Value, color=State)) + geom_line()
f2 <- ggplot(subset(housing, State %in% list("HI")), aes(x=Date, y=Home.Value, color=State)) + geom_line()
windows(xpos= 0, ypos = 0)
plot(f2)
v1 <- process("argmin", housing$State, 5, f1,f2)
x <- subset(housing, State %in% v1)
f3 <-ggplot(subset(housing, State %in% v1), aes(x=Date, y=Home.Value, color=State)) + geom_line()
windows(xpos = -1, ypos = 0)
#housing$v1 = v1
x$v1 = v1
plot(c + facet_wrap(~State))
