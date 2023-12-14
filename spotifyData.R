library(tidyverse)
library(spotifyr)


survey_songs <-
  get_playlist_audio_features("", "70RrIIkKSiARJwyQXUv30k?si=32f52496041844c4")

survey_songs$culture = c(rep("India", 5), rep("China", 5), rep("Arab", 5), rep("Western", 8))
survey_songs$modernity = c(rep("Traditional", 2), rep("Contemporary", 3), rep("Traditional", 3), rep("Contemporary", 2), rep("Traditional", 3), rep("Contemporary", 2), rep("Traditional", 3), rep("Contemporary", 5))

survey_songs |> 
  ggplot(aes(x = valence, y = energy, color = culture, shape = modernity, label = track.name)) + 
  xlim(0,1) + ylim(0,1) +
  geom_point(size=5) +
  geom_text(hjust=.5, vjust=-1.2)


top2023 <-
  get_playlist_audio_features("", "37i9dQZF1DX18jTM2l2fJY?si=53216855a85344fd")

top2023 |> 
  ggplot(aes(x = valence, y = energy, label = track.name)) + 
  geom_point(size=4) + xlim(0,1) + ylim(0,1) +
  geom_text(hjust=.1, vjust=-1)

