#############################################
# Reproducing Figure 6 of Dong etal. (2016) #
#############################################

# https://publons.com/review/518142/#c207
# Could this be explained by the original authors using a slightly different
# definition of MRAD? Instead on the y-axis they seem to plot age at death of
# oldest person alive. Hence for the period 1990-1995 there are 'missing' points
# because everyone who died in this period was still younger than the
# at-the-time oldest person alive.

# Jonas Schöley, 2016-11-03

# Init --------------------------------------------------------------------

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggrepel)

# Data --------------------------------------------------------------------

# data taken from points on Dong etal. figure 6
dong_fig_6 <- read_csv("./data/dong/data_from_dong_etal_fig6.csv", col_types = "ddc")

# grg data on verified supercentenarians
# http://www.grg.org/Adams/C.HTM
grg1 <- read_tsv("./data/grg/grg_worlds_oldest_person_titleholders.tsv",
                 skip = 1,
                 col_names = c("id", "name", "dob", "dod"),
                 col_types = c("iccc"))

grg1 %>%
  # only select persons which already died
  filter(!is.na(dod)) %>%
  # filter out disputed cases
  filter(!(id %in% c(7))) %>%
  rowwise() %>%
  mutate(
    src = "http://www.grg.org/Adams/C.HTM",
    # specify date columns
    dob = sub(pattern = "^[^[:blank:]]+ ",
              replacement = paste0(substr(dob, 1, 3), " "),
              x = dob),
    dod = sub(pattern = "^[^[:blank:]]+ ",
              replacement = paste0(substr(dod, 1, 3), " "),
              x = dod)
  ) %>% ungroup() %>%
  mutate(
    # specify date columns
    dob = mdy(dob), dod = mdy(dod),
    dob_y = year(dob), dob_m = month(dob), dob_d = day(dob),
    dod_y = year(dod), dod_m = month(dod), dod_d = day(dod),
    # add age in fractional years
    age_at_death_y = as.numeric(round((dod-dob)/365.2422, 3)),
    # Dong etal. round down to the next integer and add 0.2 years
    age_at_death_dong_rounded = floor(age_at_death_y)+0.2,
    # Dong etal. center years mid-interval (of the preceeding interval)
    dod_dong_rounded = dod_y-0.5
  ) %>% filter(dob_y <= 2015) -> grg1

# Plot --------------------------------------------------------------------

# MRADs which are not in figure 6 by Dong etal.
not_in_dong <- anti_join(x = grg1, y = dong_fig_6,
                         by = c("dod_dong_rounded" = "year", "age_at_death_dong_rounded" = "mrad"))

# plot reproduction against original
fig6_reproduce_alt <-
  ggplot(grg1, aes(x = dod_dong_rounded, y = age_at_death_dong_rounded)) +
  geom_point(size = 2.5, shape = 4) +
  geom_point(aes(x = year, y = mrad),
             shape = 21, size = 2.5,
             data = dong_fig_6) +
  geom_text_repel(aes(label = paste(name, id)),
                  segment.color = "#A7A7A7",
                  size = 3, colour = "#A7A7A7",
                  data = not_in_dong) +
  scale_y_continuous("MRAD from GRG",
                     breaks = seq(108, 124, 2),
                     limits = c(108,124),
                     expand = c(0,0)) +
  scale_x_continuous("Year",
                     breaks = seq(1950, 2020, 10),
                     limits = c(1950, 2020),
                     expand = c(0,0)) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, colour = "grey"),
        panel.grid.minor = element_blank(),
        aspect.ratio = 0.8)

ggsave("./out/fig6_reproduce_alt.pdf", fig6_reproduce_alt,
       width = 6, height = 5, scale = 1)