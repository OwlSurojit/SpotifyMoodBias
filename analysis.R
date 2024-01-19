library(tidyverse)
library(spotifyr)
library(dplyr)


spotify_data <-
  get_playlist_audio_features("", "70RrIIkKSiARJwyQXUv30k?si=32f52496041844c4") |>
  select("track.name", "energy", "valence")

spotify_data$culture = c(rep("Indian", 5), rep("Chinese", 5), rep("Arab", 5), rep("Western", 8))
spotify_data$modernity = c(rep("Traditional", 2), rep("Contemporary", 3), rep("Traditional", 3), rep("Contemporary", 2), rep("Traditional", 3), rep("Contemporary", 2), rep("Traditional", 3), rep("Contemporary", 5))


data <- read.csv("surveyData.csv")

rating_columns <- 13:58

# Filter out all clutter, all responses that did not consent, and all the empty ones
filtered_data <- data[data$Consent.Question == 1,][, rating_columns, drop=FALSE] |>
  mutate_all(as.numeric) |>
  filter(rowSums(!is.na(across(everything()))) > 0)

total_responses <- nrow(filtered_data)

# This one particular question has values from 6 to 12 for obvious reasons
filtered_data$ME3_Valence <- filtered_data$ME3_Valence - 5

# rescale 1..7 onto 0..1
scaled_data <- (filtered_data - 1) / 6
  

means <- sapply(scaled_data, mean, na.rm=TRUE)
sds <- sapply(scaled_data, sd, na.rm=TRUE)

V_range <- seq(1,46,by=2)
E_range <- seq(2,46,by=2)

human.valence <- means[V_range]
human.energy <- means[E_range]

full_data <- data.frame(spotify_data, human.energy, human.valence)
names(full_data)[2:3] <- c("spotify.energy", "spotify.valence")
row.names(full_data) <- NULL

dist_E <- full_data$spotify.energy - full_data$human.energy
dist_V <- full_data$spotify.valence - full_data$human.valence

overall_dist_E <- mean(dist_E)
overall_dist_V <- mean(dist_V)

Indian <- 1:5
Chinese <- 6:10
Arab <- 11:15
Western <- 16:23
Non_Western <- 1:15
Traditional <- c(1:2, 6:8, 11:13, 16:18)
Contemporary <- c(3:5, 9:10, 14:15, 19:23)

# Helper function that takes in a list of distances (here always dist_E and dist_V))
# and a list of style/culture ranges (defined above)
# and returns the means of these distances for all styles
calc_means_per_style <- function(dists, styles) {
  return (data.frame(
    sapply(dists, function(lst) {
      sapply(styles, function(culture) {
        mean(lst[culture])
      })
    })
  ))
}

dists_per_culture <- calc_means_per_style(list(dist_V, dist_E), list(Indian, Chinese, Arab, Western))
colnames(dists_per_culture) <- c("Valence Diff.", "Energy Diff.")
rownames(dists_per_culture) <- c("Indian", "Chinese", "Arab", "Western")

abs_dists_per_culture <- calc_means_per_style(list(abs(dist_V), abs(dist_E)), list(Indian, Chinese, Arab, Western))
colnames(abs_dists_per_culture) <- c("Valence Diff.", "Energy Diff.")
rownames(abs_dists_per_culture) <- c("Indian", "Chinese", "Arab", "Western")

dists_western_non_western <- calc_means_per_style(list(dist_V, dist_E), list(Western, Non_Western))
colnames(dists_western_non_western) <- c("Valence Diff.", "Energy Diff.")
rownames(dists_western_non_western) <- c("Western", "Non-Western")

dists_by_modernity <- calc_means_per_style(list(dist_V, dist_E), list(Traditional, Contemporary))
colnames(dists_by_modernity) <- c("Valence Diff.", "Energy Diff.")
rownames(dists_by_modernity) <- c("Traditional", "Contemporary")

dists_by_modernity_and_culture <- calc_means_per_style(list(dist_V, dist_E),
  list(intersect(Indian, Traditional), intersect(Indian, Contemporary),
       intersect(Chinese, Traditional), intersect(Chinese, Contemporary),
       intersect(Arab, Traditional), intersect(Arab, Contemporary),
       intersect(Western, Traditional), intersect(Western, Contemporary)))
colnames(dists_by_modernity_and_culture) <- c("Valence Diff.", "Energy Diff.")
rownames(dists_by_modernity_and_culture) <- c("Indian Traditional",  "Indian Contemporary",
                                              "Chinese Traditional", "Chinese Contemporary",
                                              "Arab Traditional",    "Arab Contemporary",
                                              "Western Traditional", "Western Contemporary")
  
  