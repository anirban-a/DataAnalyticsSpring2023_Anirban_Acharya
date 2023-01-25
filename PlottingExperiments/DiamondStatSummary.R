library("ggplot2")

# checking the different columns of the data set
colnames(diamonds)

ggplot(data=diamonds) +
  # Layer 1
  stat_summary(
    mapping = aes(x=cut, y=depth),
    fun.min = min,
    fun.max = max,
    fun = mean,
    color="BLUE"
  ) +
  # Layer 2
  stat_summary(
    mapping = aes(x=cut, y=depth),
    fun.min = min,
    fun.max = max,
    fun = median,
    color="GREEN"
  )