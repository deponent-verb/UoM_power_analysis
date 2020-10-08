#results script

library(tidyverse)
library(ggplot2)

pwr_df <- read_csv("./data/power_df.csv")

hr_df <- pwr_df %>% 
  filter(hs==0.21, ls==0.06) 

ggplot( hr_df, aes(x = nsam, y = power, color=season_var)) +
  geom_point() + 
  facet_grid("sample_freq")
