# Tidy Tuesday 09/04/2024
# Eclipses

# load packages
library(tidyverse)
library(gganimate)
library(maps)

# get eclipse 2024 data
tuesdata <- tidytuesdayR::tt_load('2024-04-09')
eclipse <- tuesdata$eclipse_total_2024

# get map data
us_states <- map_data("state")

# rename eclipse variables for clarity
eclipse <- eclipse %>%
  rename(contact_start = eclipse_1,
         contact_end = eclipse_6,
         half_start = eclipse_2,
         half_end = eclipse_5,
         totality_start = eclipse_3,
         totality_end = eclipse_4)

# define times to plot
pl_times <- tibble(plot_time = structure(seq(61200, 75600, 60),
                                         class = c("hms", "difftime")))

# convert eclipse data to long 
eclipse_times <- crossing(eclipse, pl_times) %>%
  mutate(in_totality  = between(plot_time, totality_start, totality_end),
         half_covered = between(plot_time, half_start, half_end),
         in_contact   = between(plot_time, contact_start, contact_end))

# rename status and remove rows with no eclipse status
eclipse_times <- eclipse_times %>%
  select(-c(contact_start:contact_end)) %>%
  mutate(ec_status = case_when(in_totality ~ "Total eclipse",
                               half_covered ~ "50+% of totality",
                               in_contact ~ "Moon in contact\nwith sun"),
         ec_status = factor(ec_status, levels = c("Moon in contact\nwith sun", "50+% of totality", "Total eclipse"))) %>%
  filter(!is.na(ec_status)) %>%
  # sort so that points closer to totality are on top
  arrange(ec_status) %>%
  # make version of plot time without seconds for display
  mutate(plot_time_hm = str_replace_all(as.character(plot_time), ":00", ""))

# plot status by time
ec_anim <- ggplot(data = eclipse_times) +
  geom_polygon(data=us_states, aes(x=long, y=lat, group=group),
               fill = "#6272a4", colour = "#454e6d", linewidth = 0.2) + 
  geom_point(aes(x = lon, y = lat, color = ec_status), alpha = 0.5, size = 1, shape = 16) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  theme_void() +
  theme(text = element_text(family = "Mukta", color = "#f8f8f2"),
        plot.title = element_text(size = 10,
                                  margin = margin(5,0,0,5)),
        plot.caption = element_text(size = 8,
                                    hjust = 0,
                                    margin = margin(0,0,0,5)),
        plot.subtitle = element_text(size = 7,
                                     margin = margin(0,0,-8,5)),
        legend.text = element_text(),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "#282A36"),
        plot.caption.position = "plot",
        legend.position = c(0.88, 0.2)
  ) +
  coord_fixed(1.3) +
  scale_colour_manual(values = c("#BD93F9", "#a4befb", "#8be9fd")) +
  transition_manual(plot_time_hm) +
  labs(title = "Eclipse 8th April 2024: extent of eclipse by time, in USA towns that experienced totality",
       subtitle = "Time: {current_frame}",
       caption = "fabp5 | Data source: NASA's Scientific Visualisation Studio",
       color = "Status") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2)))

animate(ec_anim, height = 727, width = 1200, res = 200, fps = 10, nframes = 211,
        renderer = gifski_renderer())

# export animation
anim_save("eclipse.gif")