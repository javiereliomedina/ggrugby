library(tidyverse)

team_1 <- tribble(~ID, ~x, ~y, 
                  1,  49, 45,
                  2,  49, 44,
                  3,  49, 43,
                  4,  48, 44.5,
                  5,  48, 43.5,
                  6,  48, 42.5,
                  7,  48, 45.5,
                  8,  47, 44,
                  9,  48, 47, 
                  10, 39, 35,
                  11, 20, 5,
                  12, 35, 25,
                  13, 30, 15, 
                  14, 35, 60,
                  15, 25, 50)
  
team_2 <- tribble(~ID, ~x, ~y, 
                  1,  50, 45,
                  2,  50, 44,
                  3,  50, 43,
                  4,  51, 44.5,
                  5,  51, 43.5,
                  6,  51, 42.5,
                  7,  51, 45.5,
                  8,  52, 44,
                  9,  51, 47, 
                  10, 62, 37,
                  11, 80, 12,
                  12, 63, 28,
                  13, 65, 20, 
                  14, 62, 57,
                  15, 80, 45)                                              
ggplot() +
  rugby_pitch() +
  theme_minimal() + 
  labs(title = "Rugby pitch") +
  geom_point(data = team_1, aes(x = x, y = y), col = "red") +
  geom_point(data = team_2, aes(x = x, y = y), col = "blue") 
