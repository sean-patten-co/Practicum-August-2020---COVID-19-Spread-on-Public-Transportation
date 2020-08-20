library(usmap)
library(ggplot2)

#Build a map of the US counties that are classified in the Exploration and Analysis Portion

#For Reference: plot(panelf_cluster_norm[,c("Cases_per_Person_Jun", "Avg_PMiles_Trip")], col=covid_cluster_dpm_4$cluster)
#piechart(mpg, aes(factor(1), fill = class))

#plot_usmap("counties", labels = FALSE, fill = covid_cluster_dpm_4$cluster)

#using the same data from DataBuild08192020 and ExplorationandAnalysis08192020

View(covid_cluster_dpm_4)


plot(panelf_cluster_norm[,c("days_at_home", "Restaurants")], col=covid_cluster_dpm_4$cluster)
points(covid_cluster_dpm_4$centers[,c("days_at_home", "Restaurants")], col=1:4, pch=8, cex=2)
legend(x=.9, y=1, legend = c(1,2,3,4), fill=c(1,2,3,4))

mapping_data <- cbind(covid_cluster_dpm_4$cluster, panelf_cluster_norm$FIPS) %>% as.data.frame() %>% rename(cluster = V1, fips = V2)

plot_usmap(data = mapping_data, values = "cluster")
theme(legend.position = "right")


