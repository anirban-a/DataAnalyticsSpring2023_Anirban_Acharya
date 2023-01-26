library("ggplot2")

ggplot(data=diamonds) +
  geom_boxplot(mapping = aes(x=cut, y=depth), notch = TRUE, outlier.colour = "RED")