library(tidyverse)
library(dplyr)
library(here)

################
# Load data ####
################
data <- read_delim(
  file = here("Data_satiation sinking DRYAD_2017-11-07.csv")) |>
  rename_with(~ gsub(" ","_", .x), contains(" ")) |>
  drop_na(Swarm_number) |>
  select(Swarm_number, ADCP_w_in_swarm, ADCP_w_above_midpoint, ADCP_w_below_swarm) |>
  mutate(w_outside = if_else(is.na(ADCP_w_above_midpoint), ADCP_w_below_swarm, 
                             ifelse(is.na(ADCP_w_below_swarm), ADCP_w_above_midpoint,
                             (ADCP_w_above_midpoint + ADCP_w_below_swarm) / 2),
                             )) |>
  mutate(w_anomaly = ADCP_w_in_swarm - w_outside)


data |> pivot_longer(-Swarm_number) |>
  filter(name %in% c("ADCP_w_in_swarm", "w_outside")) |>
  ggplot(aes(x=name,y=value)) +
  theme_gray(base_size = 20) +
  geom_boxplot(notch=TRUE, outlier.shape = NA) +
  scale_x_discrete(
    labels=c("ADCP_w_in_swarm" = "Inside swarm", 
             'w_outside'= 'Outside swarm')) +
  theme(axis.title.x = element_blank()) + 
  labs( y = "vertical velocity (w)")

#################
#  Git setup ####
#################
library(usethis)

