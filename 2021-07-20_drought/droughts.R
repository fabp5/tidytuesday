# Tidy Tuesday 2021-07-20
# US droughts

library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(ggthemr)
library(gganimate)
library(gifski)
library(magick)

# Get data
tuesdata <- tidytuesdayR::tt_load('2021-07-20')
drought <- tuesdata$drought
glimpse(drought)

# Filter to CA
ca <- drought %>% filter(state_abb == "CA")

# Reorder levels of drought level
ca <- ca %>%
  mutate(drought_lvl = fct_relevel(as.factor(drought_lvl), "None"))

# Name and reorder drought levels
ca <- ca %>%
  mutate(drought_lvl_name = case_when(
    drought_lvl == "None" ~ "No\ndrought",
    drought_lvl == "D0" ~ "Abnormally\ndry",
    drought_lvl == "D1" ~ "Moderate\ndrought",
    drought_lvl == "D2" ~ "Severe\ndrought",
    drought_lvl == "D3" ~ "Extreme\ndrought",
    drought_lvl == "D4" ~ "Exceptional\ndrought"
  ))

ca <- ca %>%
  mutate(drought_lvl_name = fct_relevel(as.factor(drought_lvl_name),
                                        "No\ndrought",
                                        "Abnormally\ndry",
                                        "Moderate\ndrought",
                                        "Severe\ndrought",
                                        "Extreme\ndrought",
                                        "Exceptional\ndrought"))

# Theme and colour palette for plots
ggthemr('dust', type = "outer")
pal_orange <- c("#DB735C","#E18864","#E79C6C","#EDB073","#F3C57B","#F4CE90")

# Group data into quarters and reformat
ca <- ca %>%
  mutate(quarter = quarter(valid_start, with_year = TRUE) %>%
           str_replace("\\.", "-Q"))

ca_quarter <- ca %>%
  group_by(quarter, drought_lvl, drought_lvl_name) %>%
  summarise(area_pct = mean(area_pct))

plot_q <- ca_quarter %>%
  ggplot(aes(x=drought_lvl_name,y=area_pct, fill = drought_lvl)) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values = rev(pal_orange)) +
  theme(legend.position = "none") +
  labs(title = 'Area covered by drought in California\nYear: {current_frame}',
       x = 'Drought level', y = 'Area covered (%)') +
  transition_manual(quarter)

anim_plot <- animate(plot_q, fps = 5, width = 600, height = 600)

# Order of quarters for progress bar
ca_quarter_o <- tibble(quarter = unique(ca_quarter$quarter)) %>%
  mutate(quarter_order = as.numeric(as.factor(quarter)))

# Create progress bar
progress_bar <- ggplot(ca_quarter_o, aes(x = 1, y = quarter_order)) +
  geom_bar(stat = "identity", fill = "#9A8A76") +
  coord_flip() +
  theme_void() +
  theme(panel.background = element_rect(fill = "#FAF7F2", color = "#FAF7F2")) +
  transition_manual(quarter_order)

anim_bar <- animate(progress_bar, fps = 5, width = 600, height = 25)

# Join plots and save
gif_plot <- image_read(anim_plot)
gif_bar <- image_read(anim_bar)

gif_combined <- image_append(c(gif_plot[1], gif_bar[1]), stack = TRUE)
for(i in 2:81){
  combined <- image_append(c(gif_plot[i], gif_bar[i]), stack = TRUE)
  gif_combined <- c(gif_combined, combined)
}

image_write(gif_combined, path="2021-07-20_drought/plot_droughts.gif")