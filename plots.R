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

responses_with_country <- data.frame(responses_with_country[1], scaled_data)[responses_with_country$country != "",]

means_by_country <- responses_with_country |> 
  group_by(country) |> 
  summarise(response.count=n(), across(everything(), \(x) mean(x, na.rm=T)))

# Some countries are named differently in the spdf used below
misnamed_countries <- c("Iran", "Russian Federation", "United Kingdom of Great Britain and Northern Ireland", "United States of America")
corrected_country_names <- c("Iran (Islamic Republic of)", "Russia", "United Kingdom", "United States")

means_by_country$country <- replace(means_by_country$country, means_by_country$country %in% misnamed_countries, corrected_country_names)

spotify_data_coerced = c(rbind(spotify_data$valence, spotify_data$energy))

dists_by_country <- means_by_country
dists_by_country[,rating_columns-10] <- -sweep(dists_by_country[,rating_columns-10], 2, spotify_data_coerced)


dists_by_country_and_culture <- data.frame(dists_by_country[1:2],
    Valence.Indian  = rowMeans(dists_by_country[Indian*2+1]),
    Energy.Indian   = rowMeans(dists_by_country[Indian*2+2]),
    Valence.Chinese = rowMeans(dists_by_country[Chinese*2+1]),
    Energy.Chinese  = rowMeans(dists_by_country[Chinese*2+2]),
    Valence.Arab    = rowMeans(dists_by_country[Arab*2+1]),
    Energy.Arab     = rowMeans(dists_by_country[Arab*2+2]),
    Valence.Western = rowMeans(dists_by_country[Western*2+1]),
    Energy.Western  = rowMeans(dists_by_country[Western*2+2])
  )

combined_dists_country_culture <- data.frame(dists_by_country[1:2],
    Distance.Indian = sqrt(dists_by_country_and_culture$Valence.Indian^2 + dists_by_country_and_culture$Energy.Indian^2),
    Distance.Chinese = sqrt(dists_by_country_and_culture$Valence.Chinese^2 + dists_by_country_and_culture$Energy.Chinese^2),
    Distance.Arab = sqrt(dists_by_country_and_culture$Valence.Arab^2 + dists_by_country_and_culture$Energy.Arab^2),
    Distance.Western = sqrt(dists_by_country_and_culture$Valence.Western^2 + dists_by_country_and_culture$Energy.Western^2)
  )


# load country border data
library(sf)
library(leaflet)
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="plot_data/world_borders.zip")
unzip("plot_data/world_borders.zip", exdir="plot_data")

# Read shape file into spatial polygon data frame
countries_spdf <- read_sf(dsn = "plot_data", layer = "TM_WORLD_BORDERS_SIMPL-0.3")


# Add our data to the spdf
full_spdf <- merge(countries_spdf, combined_dists_country_culture, by.x = "NAME", by.y = "country", all.x=TRUE) |>
  merge(dists_by_country_and_culture[,-2], by.x = "NAME", by.y="country", all.x=TRUE)


map_colour_palette <- function (valence, energy, total) 
{
  rng <- range(total, na.rm = TRUE)
  rescaled <- scales::rescale(total, from = rng)
  if (any(rescaled < 0 | rescaled > 1, na.rm = TRUE)) 
    warning("Some values were outside the color scale and will be treated as NA")

  ifelse(is.na(rescaled), "transparent",
         sapply(rescaled, function(x) ifelse(!is.na(x), grDevices::hsv(h=0, s=x, v=1), NA)))
}


world_map_tooltips <- function(culture_str) {
  paste0(
  "Country: ", full_spdf$NAME,"<br/>", 
  ifelse(is.na(full_spdf$response.count), 
  "No Data<br/>", 
  paste0(
    "№ Responses: ", full_spdf$response.count, "<br/>", 
    "Difference in Valence: ", round(full_spdf[[paste0("Valence.", culture_str)]], 4), "<br/>",
    "Difference in Energy: ", round(full_spdf[[paste0("Energy.", culture_str)]], 4), "<br/>",
    "Total Difference: ", round(full_spdf[[paste0("Distance.", culture_str)]], 4), "<br/>"
    )
  )) |>
  lapply(htmltools::HTML)
}

map_label_options <- labelOptions( 
  style = list("font-weight" = "normal", padding = "3px 8px"), 
  textsize = "13px", 
  direction = "auto"
)

addPolygons_custom <- function(x, culture_str) {
  colour_palette <- map_colour_palette(
    full_spdf[[paste0("Valence.", culture_str)]],
    full_spdf[[paste0("Energy.", culture_str)]],
    full_spdf[[paste0("Distance.", culture_str)]]
  )
  palette_range
  addPolygons(x,
    group = culture_str,
    fillColor = colour_palette, 
    fillOpacity = 0.9,
    stroke=FALSE, 
    label=world_map_tooltips(culture_str),
    labelOptions = map_label_options) |>
    addLegend(
      group = culture_str,
      colors= colour_palette, 
      values=~full_spdf[[paste0("Distance.", culture_str)]], 
      opacity=0.9, 
      title = "Rating Distance", 
      position = "bottomleft" )
}

# Basic choropleth with leaflet?
leaflet_widget <- leaflet(full_spdf) |>
  addTiles()  |>
  setView(lat=10, lng=0 , zoom=2) |>
  addPolygons_custom("Indian") |>
  addPolygons_custom("Chinese") |>
  addPolygons_custom("Arab") |>
  addPolygons_custom("Western") |>
  addLayersControl(baseGroups = c("Indian", "Chinese", "Arab", "Western"), 
                   options = layersControlOptions(collapsed = FALSE)) 
  

leaflet_widget
