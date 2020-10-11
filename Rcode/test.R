library(tidyverse)

team_1 <- tribble(~ID, ~x, ~y, 
                  1,  50, 45,
                  2,  50, 44,
                  3,  50, 43,
                  4,  49, 44.5,
                  5,  49, 43.5,
                  6,  49, 42.5,
                  7,  49, 45.5,
                  8,  48, 44,
                  9,  49, 47, 
                  10, 40, 35,
                  11, 20, 5,
                  12, 35, 25,
                  13, 30, 15, 
                  14, 35, 60,
                  15, 25, 50)
                                                 
ggplot() +
  rugby_pitch() +
  theme_minimal() + 
  labs(title = "Rugby ") +
  geom_point(data = team_1, aes(x = x, y = y), col = "red") 
