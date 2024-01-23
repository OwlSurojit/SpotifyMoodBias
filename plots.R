library(tidyverse)
library(plotly)

modernity = rep(c("Traditional", "Contemporary"), 4)
culture = c(rep("Indian", 2), rep("Chinese", 2), rep("Arab", 2), rep("Western", 2))
main_graph_data <- data.frame(dists_by_modernity_and_culture, modernity, culture)

# main_graph <- main_graph_data |>
#   ggplot(aes(x=`Valence.Difference`, y=`Energy.Difference`, color = culture, shape=modernity)) + 
#   geom_segment(aes(x=0, y=0, xend=`Valence.Difference`, yend=`Energy.Difference`), 
#                arrow = arrow(length = unit(0.2, "inches"))) +
#   geom_point(size=3) + xlim(-0.5, 0.5) + ylim(-0.5,0.5) +
#   scale_color_manual(values = c("Indian" = "red", "Chinese" = "green", "Arab" = "blue", "Western" = "purple")) +
#   scale_shape_manual(values = c("Contemporary" = 16, "Traditional" = 17)) +
#   labs(color = "Culture", shape = "Modernity")
# plot(main_graph)


main_graph <- plot_ly(main_graph_data, x = ~`Valence.Diff.`, y = ~`Energy.Diff.`, color = ~culture, symbol = ~modernity,
             colors = c("red", "green", "blue", "purple"),
             symbols = c("x", "circle"),
             type = 'scatter', mode = 'markers', size=10) |>
  layout(title = "Differences between Human and Spotify ratings per culture and modernity",
         xaxis = list(range=c(-0.5,0.5), title="Difference in Valence"), yaxis = list(range=c(-0.5, 0.5), title="Difference in Energy"))

main_graph



#####################
##### WORLD MAP #####
#####################

# prepare survey data
responses_with_country <- data[data$Consent.Question == 1,][, c(12,rating_columns), drop=FALSE] |>
  mutate(across(rating_columns - 11, as.numeric)) |>
  filter(rowSums(!is.na(across(rating_columns - 11))) > 0)

responses_with_country <- data.frame(country = responses_with_country$country, scaled_data)[responses_with_country$country != "",]

means_by_country <- responses_with_country |> 
  group_by(country) |> 
  summarise(count=n(), across(everything(), \(x) mean(x, na.rm=T)))

spotify_data_coerced = c(rbind(spotify_data$valence, spotify_data$energy))

dists_by_country <- means_by_country
dists_by_country[,rating_columns-10] <- -sweep(dists_by_country[,rating_columns-10], 2, spotify_data_coerced)


dists_by_country_and_culture <- data.frame(dists_by_country[1:2],
    Indian.Valence  = rowMeans(dists_by_country[Indian*2+1]),
    Indian.Energy   = rowMeans(dists_by_country[Indian*2+2]),
    Chinese.Valence = rowMeans(dists_by_country[Chinese*2+1]),
    Chinese.Energy  = rowMeans(dists_by_country[Chinese*2+2]),
    Arab.Valence    = rowMeans(dists_by_country[Arab*2+1]),
    Arab.Energy     = rowMeans(dists_by_country[Arab*2+2]),
    Western.Valence = rowMeans(dists_by_country[Western*2+1]),
    Western.Energy  = rowMeans(dists_by_country[Western*2+2])
  )

combined_dists_country_culture <- data.frame(dists_by_country[1:2],
    Indian = dists_by_country_and_culture$Indian.Valence^2 + dists_by_country_and_culture$Indian.Energy^2,
    Chinese = dists_by_country_and_culture$Chinese.Valence^2 + dists_by_country_and_culture$Chinese.Energy^2,
    Arab = dists_by_country_and_culture$Arab.Valence^2 + dists_by_country_and_culture$Arab.Energy^2,
    Western = dists_by_country_and_culture$Western.Valence^2 + dists_by_country_and_culture$Western.Energy^2
  )


# load country border data
install.packages(c("sf", "leaflet"))
library(sf)
library(leaflet)
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="plot_data/world_borders.zip")
unzip("plot_data/world_borders.zip", exdir="plot_data")

# Read shape file into spatial polygon data frame
countries_spdf <- read_sf(dsn = "plot_data", layer = "TM_WORLD_BORDERS_SIMPL-0.3")

mypalette <- colorNumeric( palette="viridis", domain=countries_spdf$POP2005, na.color="transparent")
mypalette(c(45,43))

# Basic choropleth with leaflet?
leaflet_widget <- leaflet(countries_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( fillColor = ~mypalette(POP2005), stroke=FALSE )

leaflet_widget
