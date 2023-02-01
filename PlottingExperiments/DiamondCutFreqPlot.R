library("ggplot2")

# simple diamond cut vs count plot
ggplot(data=diamonds) +
  geom_bar(mapping = aes(x=cut))

# diamond cut vs count plot, by clarity
ggplot(data=diamonds) +
  geom_bar(mapping = aes(x=cut, fill=clarity))

# bars stacked side-by-side by clarity
ggplot(data=diamonds) +
  geom_bar(mapping = aes(x=cut, fill=clarity), position = "dodge")