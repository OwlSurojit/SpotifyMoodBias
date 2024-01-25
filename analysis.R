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

## statistical analysis

## Data frame
# track name
track_names <- rep(spotify_data$track.name, each = total_responses)

# Valence and Energy - human ratings
V_range <- seq(1,46,by=2)
E_range <- seq(2,46,by=2)

scaled_data_V <- scaled_data[,V_range]
scaled_data_E <- scaled_data[,E_range]

Valence_human <- data.frame(unlist(scaled_data_V))[,1]
Energy_human <- data.frame(unlist(scaled_data_E))[,1]

# region
Indian <- 1:5
Chinese <- 6:10
Arab <- 11:15
Western <- 16:23

regions <- c("Indian", "Chinese", "Arab", "Western")
region <- c(rep(regions, each = 5*total_responses), rep("Western", each = 3*total_responses))

# western / non-western
West_non_west <- c(rep("non-Western", each = 3*5*total_responses), rep("Western", each = 8*total_responses))

# traditional / contemporary
Traditional <- c(1:2, 6:8, 11:13, 16:18)
Contemporary <- c(3:5, 9:10, 14:15, 19:23)

tc1 <- character()
tc1[Traditional] <- "traditional"
tc1[Contemporary] <- "contemporary"

traditional_contemporary <- rep(tc1, each = total_responses)

# spotify rating
Valence_spotify <- rep(spotify_data$valence, each = total_responses)
Energy_spotify <- rep(spotify_data$energy, each = total_responses)

# distance
Valence_distance <- Valence_spotify - Valence_human
Energy_distance <- Energy_spotify - Energy_human

Valence_distance_abs <- abs(Valence_distance)
Energy_distance_abs <- abs(Energy_distance)


# dataframe
big_data <- data.frame(track_names, Valence_distance, Energy_distance, Valence_distance_abs, Energy_distance_abs, region, West_non_west, traditional_contemporary, Valence_human, Energy_human, Valence_spotify, Energy_spotify)

## Analysis
## Absolute distance values
# Valence: two way ANOVA: region x tradition
ANOVA_V1 <- aov(Valence_distance_abs ~ region * traditional_contemporary,
                data = big_data)
summary(ANOVA_V1)

# Energy: two way ANOVA: region x tradition
ANOVA_E1 <- aov(Energy_distance_abs ~ region * traditional_contemporary,
                data = big_data)
summary(ANOVA_E1)

# Valence: two-way ANOVA: west/nonwest x tradition
ANOVA_V2 <- aov(Valence_distance_abs ~ West_non_west * traditional_contemporary,
                data = big_data)
summary(ANOVA_V2)

# Energy: two way ANOVA: west/nonwest x tradition
ANOVA_E2 <- aov(Energy_distance_abs ~ West_non_west * traditional_contemporary,
                data = big_data)
summary(ANOVA_E2)

# Valence: ANOVA: region
ANOVA_V3 <- aov(Valence_distance_abs ~ region,
                data = big_data)
summary(ANOVA_V3)

# Energy: ANOVA: region
ANOVA_E3 <- aov(Energy_distance_abs ~ region,
                data = big_data)
summary(ANOVA_E3)

# Energy: follow-up pairwise t-tests
library("rstatix")
pairwise_t_test(Energy_distance_abs ~ region,
                data = big_data,
                p.adjust.method = "bonferroni")

# Valence: t-test: west/non-west
t.test(Valence_distance_abs ~ West_non_west, data = big_data)

# Energy: t-test: west/non-west
t.test(Energy_distance_abs ~ West_non_west, data = big_data)

# Valence: t-test: tradition
t.test(Valence_distance_abs ~ traditional_contemporary, data = big_data)

# Energy: t-test: tradition
t.test(Energy_distance_abs ~ traditional_contemporary, data = big_data)

