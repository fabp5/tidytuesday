# Tidy Tuesday 2021-05-11
# US broadband access

library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(maps)
library(scales)
library(cowplot)

# Get data
tuesdata <- tidytuesdayR::tt_load('2021-05-11')
broadband <- tuesdata$broadband
broadband <- clean_names(broadband)

# Filter to Texas
tx_bb <- broadband %>% filter(st == "TX")
tx_bb$county_short <- str_replace(tx_bb$county_name," County", "") %>% tolower()

# Get county map data
tx <- map_data("county","texas")

# Check any counties that don't match
anti_join(tx_bb,tx,by=c("county_short"="subregion"))

# Rename De Witt county
tx_bb$county_short[tx_bb$county_short == "dewitt"] <- "de witt"

# Join broadband and map data
tx_bb_map <- left_join(tx,tx_bb,by=c("subregion" = "county_short"))

# Convert broadband data to numeric
tx_bb_map$broadband_availability_per_fcc <- as.numeric(tx_bb_map$broadband_availability_per_fcc)
tx_bb_map$broadband_usage <- as.numeric(tx_bb_map$broadband_usage)

# Make plots
plot_tx_availability <- 
  ggplot() + 
  geom_polygon(data=tx_bb_map,
               aes(x=long, y=lat, fill=broadband_availability_per_fcc, group=group),
               color="white") +
  coord_fixed(1.3) +
  theme_void() +
  labs(fill=' ') +
  scale_fill_gradient(low = "#c7e0f5", high = "#29567a", na.value="grey80", labels = percent_format(),
                      guide = guide_colourbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE),
                      limits=c(0,1)) +
  labs(caption="Broadband availability") + 
  theme(plot.caption = element_text(hjust=1, size=rel(1.2)))

plot_tx_usage <- 
  ggplot() + 
  geom_polygon(data=tx_bb_map,
               aes(x=long, y=lat, fill=broadband_usage, group=group),
               color="white") +
  coord_fixed(1.3) +
  theme_void() +
  labs(fill=' ') +
  scale_fill_gradient(low = "#c7f5c8", high = "#297a2a", na.value="grey80", labels = percent_format(),
                      guide = guide_colourbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE),
                      limits=c(0,1)) +
  labs(caption="Broadband usage") + 
  theme(plot.caption = element_text(hjust=1, size=rel(1.2)))

plot_row <- plot_grid(plot_tx_availability, plot_tx_usage)

title <- ggdraw() + 
  draw_label("Broadband availability and usage in Texas (2019)",
             fontface = 'bold',
             x = 0,
             hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

plot_grid(title, plot_row,
          ncol = 1,
          rel_heights = c(0.08, 1))

# Export plot
ggsave("2021-05-11_broadband/texas_broadband.png", dpi = 100, height = 6, width = 12.4)
