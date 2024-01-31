library(tidyverse)
library(plotly)

######################
### RAW DATA PLOTS ###
######################


spotify_mood_data <- spotify_data |> 
  ggplot(aes(x = valence, y = energy, color = culture, shape = modernity, label = track.name)) + 
  xlim(0,1) + ylim(0,1) +
  geom_point(size=3)
ggplotly(spotify_mood_data, tooltip = c("track.name"))

culture_colours = c("#00BFC4", "#F7766D", "#C77CFF", "#7CAE00")

spotify_mood_data_plot <- plot_ly(
  full_data, 
  x = ~spotify.valence,
  y = ~spotify.energy,
  color = ~culture,
  symbol = ~modernity,
  colors = culture_colours,
  symbols = c("x", "circle"),
  type = 'scatter', 
  mode = 'markers', 
  size=10,
  text = ~paste(track.name, "●", artists),
  #hoverinfo = "text+x+y") |>
  hovertemplate = paste("<b>%{text}</b><br>",
                         "Valence: %{x}<br>",
                         "Energy: %{y}",
                         "<extra></extra>")
  ) |>
  layout(title = "Spotify Mood Data",
         xaxis = list(range=c(-0.05,1.05), title="Valence", showline=FALSE, zeroline = FALSE, gridcolor="white"), 
         yaxis = list(range=c(-0.05,1.05), title="Energy", showline=FALSE, zeroline = FALSE, gridcolor="white"), 
         plot_bgcolor = "#EBEBEB"
         )
spotify_mood_data_plot


human_mood_data_plot <- plot_ly(
  full_data, 
  x = ~human.valence,
  y = ~human.energy,
  color = ~culture,
  symbol = ~modernity,
  colors = culture_colours,
  symbols = c("x", "circle"),
  type = 'scatter', 
  mode = 'markers', 
  size=10,
  text = ~paste(track.name, "●", artists),
  #hoverinfo = "text+x+y") |>
  hovertemplate = paste("<b>%{text}</b><br>",
                        "Valence: %{x}<br>",
                        "Energy: %{y}",
                        "<extra></extra>")
) |>
  layout(title = "Human Mood Data",
         xaxis = list(range=c(-0.05,1.05), title="Valence", showline=FALSE, zeroline = FALSE, gridcolor="white"), 
         yaxis = list(range=c(-0.05,1.05), title="Energy", showline=FALSE, zeroline = FALSE, gridcolor="white"), 
         plot_bgcolor = "#EBEBEB"
  )
human_mood_data_plot




#####################
##### MAIN PLOT #####
#####################

modernity = rep(c("Traditional", "Contemporary"), 4)
culture = c(rep("Indian", 2), rep("Chinese", 2), rep("Arab", 2), rep("Western", 2))

# main_graph <- main_graph_data |>
#   ggplot(aes(x=`Valence.Difference`, y=`Energy.Difference`, color = culture, shape=modernity)) + 
#   geom_segment(aes(x=0, y=0, xend=`Valence.Difference`, yend=`Energy.Difference`), 
#                arrow = arrow(length = unit(0.2, "inches"))) +
#   geom_point(size=3) + xlim(-0.5, 0.5) + ylim(-0.5,0.5) +
#   scale_color_manual(values = c("Indian" = "red", "Chinese" = "green", "Arab" = "blue", "Western" = "purple")) +
#   scale_shape_manual(values = c("Contemporary" = 16, "Traditional" = 17)) +
#   labs(color = "Culture", shape = "Modernity")
# plot(main_graph)

human_ratings_culture_modernity <- calc_means_per_style(list(full_data$human.valence, full_data$human.energy), modernity_culture_indices)
colnames(human_ratings_culture_modernity) <- c("Valence.Human", "Energy.Human")

spotify_ratings_culture_modernity <- calc_means_per_style(list(full_data$spotify.valence, full_data$spotify.energy), modernity_culture_indices) 
colnames(spotify_ratings_culture_modernity) <- c("Valence.Spotify", "Energy.Spotify")

main_graph_data <- data.frame(modernity, 
                              culture, 
                              Song.Type = interaction(culture, modernity, sep = " "),
                              dists_by_modernity_and_culture,
                              human_ratings_culture_modernity, 
                              spotify_ratings_culture_modernity,
                              Overall.Diff = sqrt(dists_by_modernity_and_culture$Valence.Diff^2 + dists_by_modernity_and_culture$Energy.Diff^2))


main_graph <- plot_ly() |>
  add_trace(data = main_graph_data,
   x = ~Valence.Diff,
   y = ~Energy.Diff,
   xaxis = 'x',
   yaxis = 'y',
   color = ~culture,
   symbol = ~modernity,
   colors = culture_colours,
   symbols = c("x", "circle"),
   type = 'scatter', 
   mode = 'markers', 
   size = 10, 
   hovertemplate = ~paste0("<b>", culture, " ", modernity, "</b><br>",
                           "Valence Spotify: ", round(Valence.Spotify, 4), "<br>",
                           "Energy Spotify: ", round(Energy.Spotify, 4), "<br>",
                           "Valence Human: ", round(Valence.Human, 4), "<br>",
                           "Energy Human: ", round(Energy.Human, 4), "<br>",
                           "Valence Difference: %{x:.4f}<br>",
                           "Energy Difference: %{y:.4f}<br>",
                           "<i>Overall Difference:</i> ", round(Overall.Diff, 4),
                           "<extra></extra>")
   ) |>
  add_trace(data = main_graph_data,
            x = ~Song.Type,
            y = ~Overall.Diff,
            color = ~culture,
            colors = culture_colours,
            type = 'bar',
            xaxis = 'x2',
            yaxis = 'y2',
            visible=F,
            inherit=F,
            hovertemplate = ~paste0("<b>", culture, " ", modernity, "</b><br>",
                                    "Valence Spotify: ", round(Valence.Spotify, 4), "<br>",
                                    "Energy Spotify: ", round(Energy.Spotify, 4), "<br>",
                                    "Valence Human: ", round(Valence.Human, 4), "<br>",
                                    "Energy Human: ", round(Energy.Human, 4), "<br>",
                                    "Valence Difference: %{x:.4f}<br>",
                                    "Energy Difference: %{y:.4f}<br>",
                                    "<i>Overall Difference:</i> ", round(Overall.Diff, 4),
                                    "<extra></extra>")
            ) |>
  layout(title = "Differences between Human and Spotify ratings per culture and modernity",
         margin = list(b=80),
         xaxis = list(range=c(-0.5, 0.5), title="Difference in Valence"), 
         yaxis = list(range=c(-0.5, 0.5), title="Difference in Energy"),
         xaxis2 = list(overlaying = "x", visible = FALSE, type='category'),
         yaxis2 = list(overlaying = "y", visible = FALSE),
         updatemenus = list(
           list(
             type = "buttons",
             direction = "down",
             xanchor = "left",
             yanchor = "top",
             pad = list(l= 10, r=10, t= 10, b = 10),
             x = 0,
             y = 1,
             buttons = list(
               list(method = "update",
                    args = list(list(visible = list(T,T,T,T,T,T,T,T,F,F,F,F)),
                                list(xaxis = list(visible = TRUE,
                                                  range=c(-0.5, 0.5), title="Difference in Valence"),
                                     xaxis2 = list(overlaying = "x", visible = FALSE),
                                     yaxis = list(visible = TRUE,
                                                  range=c(-0.5, 0.5), title="Difference in Energy"),
                                     yaxis2 = list(overlaying = "y", visible = FALSE))),
                    label = "Scatter"),
               list(method = "update",
                    args = list(list(visible = list(F,F,F,F,F,F,F,F,T,T,T,T)),
                                list(xaxis = list(visible = F),
                                     xaxis2 = list(overlaying = "x", visible = T),
                                     yaxis = list(visible = F),
                                     yaxis2 = list(overlaying = "y", visible = T,
                                                   title="Overall Difference")),
                                list(showlegend = F)),
                    label = "Barchart")
             ))
         ))

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
# Uncomment this to get the world map border data
# download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="plot_data/world_borders.zip")
# unzip("plot_data/world_borders.zip", exdir="plot_data")

# Read shape file into spatial polygon data frame
countries_spdf <- read_sf(dsn = "plot_data", layer = "TM_WORLD_BORDERS_SIMPL-0.3")


# Add our data to the spdf
full_spdf <- merge(countries_spdf, combined_dists_country_culture, by.x = "NAME", by.y = "country", all.x=TRUE) |>
  merge(dists_by_country_and_culture[,-2], by.x = "NAME", by.y="country", all.x=TRUE)


culture_hues <- c("Indian" = 274, "Chinese" = 3, "Arab" = 190, "Western" = 100)
culture_lightness <- c("Indian" = .76, "Chinese" = .7, "Arab" = .74, "Western" = .68)

map_colour_palette <- function (culture_str, total) 
{
  rng <- range(total, na.rm = TRUE)
  rescaled <- scales::rescale(total, from = rng)

  cols <- ifelse(is.na(rescaled), "transparent",
         sapply(rescaled, 
                function(x) ifelse(!is.na(x), 
                                   grDevices::hsv(
                                     h=culture_hues[culture_str]/360, 
                                     s=x, 
                                     v=0.7), 
                                   NA)))
  lbls <- seq(rng[1], rng[2], length.out=5)
  pal <- grDevices::hsv(
    h=culture_hues[culture_str]/360, 
    s=scales::rescale(lbls, from = rng), 
    v=0.7)
  
  list("palette" = pal, "country.colours" = cols, "labels" = round(lbls, 4))
}


world_map_tooltips <- function(culture_str) {
  paste0(
  "<b>", full_spdf$NAME,"</b><br/>", 
  ifelse(is.na(full_spdf$response.count), 
  "No Data<br/>", 
  paste0(
    "№ Responses: ", full_spdf$response.count, "<br/>", 
    "Difference in Valence: ", round(full_spdf[[paste0("Valence.", culture_str)]], 4), "<br/>",
    "Difference in Energy: ", round(full_spdf[[paste0("Energy.", culture_str)]], 4), "<br/>",
    "<i>Total Difference:</i> ", round(full_spdf[[paste0("Distance.", culture_str)]], 4), "<br/>"
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
    culture_str,
    full_spdf[[paste0("Distance.", culture_str)]]
  )
  addPolygons(x,
    group = culture_str,
    fillColor = colour_palette$country.colours, 
    fillOpacity = 0.9,
    stroke=FALSE, 
    label=world_map_tooltips(culture_str),
    labelOptions = map_label_options) |>
    addLegend(
      group = culture_str,
      className = paste("info legend", culture_str),
      colors= colour_palette$palette,
      labels= colour_palette$labels,
      values=~full_spdf[[paste0("Distance.", culture_str)]],
      opacity=0.9,
      title = "Total Rating Difference",
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
                   options = layersControlOptions(collapsed = FALSE))  |>
  # Custom JS to show only the relevant legends
  htmlwidgets::onRender("
      function(el, x) {
         var updateLegend = function () {

            var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

            document.querySelectorAll('.legend').forEach(a => a.hidden=true);
            document.querySelectorAll('.legend').forEach(l => {
               if (l.classList.contains(selectedGroup)) {l.hidden=false; console.log(selectedGroup)};
            });
         };
         updateLegend();
         window.addEventListener('load', () => {
          console.log('helo again');
          updateLegend();
         });
         this.on('baselayerchange', el => updateLegend());
      }"
  )
  

leaflet_widget
