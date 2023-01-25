not_landlocked <- !epi_data$Landlock
hist(epi_data[not_landlocked, "EPI"], rm.na=TRUE, xlab = "EPI", main = "Histogram of EPI value for non-landlocked nations")

# plot histogram for EPI value
hist(epi_data[not_landlocked, "EPI"], rm.na=TRUE, xlab = "EPI", breaks = seq(30,100,0.5), main = "Histogram of EPI value for non-landlocked nations")
hist(epi_data[not_landlocked, "EPI"], rm.na=TRUE, xlab = "EPI", breaks = seq(30,100,0.5), main = "Histogram of EPI value for non-landlocked nations", probability = TRUE)

library(ggplot2)

ggplot(data = epi_data) +
  geom_point(mapping = aes(EPI, as.logical(No_surface_water), color=No_surface_water)) +
  labs(y="No Surface Water", x="EPI") + xlim(37,78.5)

ggplot(data = epi_data) +
  geom_point(mapping = aes(as.logical(No_surface_water),GEO_subregion, color=No_surface_water)) +
  labs(x="No Surface Water", y="Geo sub-regions")

ggplot(data = epi_data) +
  geom_point(mapping = aes(as.logical(No_surface_water), EPI_regions, color=No_surface_water)) +
  labs(x="No Surface Water", y="EPI Regions")



# Let's check the distribution of population density..
boxplot(epi_data$PopulationDensity, notch = TRUE)

# population density vs EPI, categorized by Desert:
ggplot(data = epi_data) +
  geom_point(mapping = aes(EPI, PopulationDensity))+
  facet_wrap(~ as.logical(Desert))