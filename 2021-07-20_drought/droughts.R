# Tidy Tuesday 2021-07-20
# US droughts

library(tidytuesdayR)
library(tidyverse)
library(ggthemr)
library(gganimate)
library(gifski)

# Get data
tuesdata <- tidytuesdayR::tt_load('2021-07-20')
drought <- tuesdata$drought
glimpse(drought)

# Filter to CA
ca <- drought %>% filter(state_abb == "CA")