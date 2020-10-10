library(tidyverse)

shots_data <- tibble(x = c(90, 85, 82, 78, 83),
                         y = c(43, 40, 52, 56, 44))
                                                 
ggplot(shots_data, aes(x = x, y = y)) +
  annotate_pitch() +
  theme_pitch() +
  geom_point() 
