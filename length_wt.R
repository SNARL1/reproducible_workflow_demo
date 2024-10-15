# load packages
library(tidyverse)
library(lubridate)
library(tidyr)

# read in data
deso_frogs <- read_csv(here::here("data", "deso_capture.csv"))

# data wrangling
deso_frogs_clean <- deso_frogs %>% 
  mutate(across(where(is.character), tolower)) %>% 
  filter(frog_state != "dead") %>% 
  filter(frog_svl > 30) %>% 
  drop_na(frog_svl) %>%
  drop_na(frog_sex) %>% 
  filter(frog_weight > 5) %>% 
  drop_na(frog_weight) %>% 
  mutate(capture_date = as_date(format(survey_date, "%Y-%m-%d"))) %>% 
  select(site_id, capture_date, frog_sex, frog_svl, frog_weight)

# plot weight versus length
len_wt_plot <- deso_frogs_clean %>% 
  ggplot(aes(x = frog_svl, y = frog_weight, color = frog_sex)) +
  geom_point() +
  scale_x_continuous(transform = "log10") +
  scale_y_continuous(transform = "log10") + 
  labs(x = "Snout-vent length (mm)", y = "Weight (g)") +
  geom_smooth(method = lm) +
  scale_color_manual(values = c("blue", "green"))

len_wt_plot

ggsave(here::here("out", "len_wt_plot.png"))
