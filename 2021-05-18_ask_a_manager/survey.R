# Tidy Tuesday 2021-05-18
# Ask a Manager Survey

library(tidyverse)
library(ggridges)

# Get data
tuesdata <- tidytuesdayR::tt_load('2021-05-18')
survey <- tuesdata$survey

# Filter to UK respondents
survey_uk <- survey %>%
  filter(grepl("kingdom|uk|britain|eng|wales|scot",country,ignore.case = TRUE))
table(survey_uk$country)
survey_uk <- survey_uk %>%
  filter(country != "Ukraine")

# Filter to only those paid in GBP
survey_uk <- survey_uk %>%
  filter(currency == "GBP")

# Reorder gender levels
survey_uk$gender_f <- factor(survey_uk$gender,
                             levels = c("Other or prefer not to answer", "Non-binary", "Woman", "Man"))

# Create colour palette
pal_green <- c("#A0E5FA", "#8CDBE3", "#79D1CD", "#69c9b7", "#6bc7ad", "#67cba6")

# Violin plot by gender
survey_uk %>%
  filter(!is.na(gender_f)) %>%
  ggplot(aes(x = annual_salary, y = gender_f, fill = gender_f)) +
  geom_violin(size = 1, colour = "#444444") +
  geom_boxplot(width=0.1,outlier.shape = NA, size = 1, colour = "#444444") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 250000)) +
  scale_fill_manual(values=pal_green) +
  theme(axis.title.x = element_text(size = 14, vjust = -0.4),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5)) +
  labs(x = "Annual salary (GBP)",
       title = expression(paste("Salary of UK respondents to ", italic("Ask a Manager"), " survey")),
       subtitle = "By gender") +
  scale_x_continuous(labels = function(x) format(x, big.mark = ",",scientific = FALSE))

ggsave("2021-05-18_ask_a_manager/survey_salaries_gender.png", dpi = 100, height = 8.5, width = 14)

# Reorder education levels
survey_uk$highest_level_of_education_completed_f <-
  factor(survey_uk$highest_level_of_education_completed,
         levels = c("High School","Some college","College degree",
                    "Professional degree (MD, JD, etc.)","Master's degree","PhD"))

# Violin plot with boxplots
# Filter to men and women as other categories have too few entries for visualisation
survey_uk %>%
  filter(
    gender == "Man" | gender == "Woman",
    !is.na(highest_level_of_education_completed_f)) %>%
  mutate(gender = case_when(gender == "Man" ~ "Men",
                            gender == "Woman" ~ "Women")) %>%
  ggplot(aes(x = annual_salary, y = highest_level_of_education_completed_f,
             fill = highest_level_of_education_completed_f)) +
  geom_violin(size = 1, colour = "#444444") +
  geom_boxplot(width=0.1,outlier.shape = NA, size = 1, colour = "#444444") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 250000)) +
  facet_grid(cols = vars(gender)) +
  scale_fill_manual(values=pal_green) +
  theme(axis.title.x = element_text(size = 14, vjust = -0.4),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5)) +
  labs(x = "Annual salary (GBP)",
       title = expression(paste("Salary of UK respondents to ", italic("Ask a Manager"), " survey")),
       subtitle = "By gender and education level") +
  scale_x_continuous(labels = function(x) format(x, big.mark = ",",scientific = FALSE))

# Export plot
ggsave("2021-05-18_ask_a_manager/survey_salaries_gender_edu.png", dpi = 100, height = 8.5, width = 14)
