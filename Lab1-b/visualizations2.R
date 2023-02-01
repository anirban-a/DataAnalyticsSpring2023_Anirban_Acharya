plot(mtcars$wt, mtcars$mpg)


# plotting the same with ggplot.
ggplot(data=mtcars)+
  geom_point(aes(wt, mpg))

ggplot(data=mtcars)+
  geom_point(aes(wt, mpg))+facet_wrap(~mtcars$hp, nrow = 3)

plot(pressure$temperature, pressure$pressure, type = "l")

lines(pressure$temperature, pressure$pressure/2, col="red")
points(pressure$temperature, pressure$pressure/2, col="blue")

qplot(pressure$temperature, pressure$pressure, geom = "line")

# using ggplot to plot the above graph
ggplot(data = pressure)+
  geom_line(aes(x=temperature, y=pressure))+
  geom_line(aes(x=temperature, y=pressure/2), col="red")+
  geom_point(aes(x=temperature, y=pressure/2), col="blue")

barplot(BOD$demand, names.arg = BOD$Time)

barplot(table(mtcars$cyl))

table(mtcars$cyl)

ggplot(mtcars)+geom_bar(aes(as.factor(mtcars$cyl)))