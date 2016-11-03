#############################################
# Reproducing Figure 6 of Dong etal. (2016) #
#############################################

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
# http://www.grg.org/Adams/A.HTM
grg1 <- read_tsv("./data/grg/grg_verified_supercentenarians.tsv",
                 skip = 1,
                 col_names = c("name", "dob", "dod"),
                 col_types = c("-ccc---------------"))

grg1 %>%
  # only select persons which already died
  filter(!is.na(dod)) %>%
  rowwise() %>%
  mutate(
    src = "http://www.grg.org/Adams/A.HTM",
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
  ) -> grg1

# grg data on validated supercentenarian deaths since 2008
# http://www.grg.org/SC/WorldSCRankingsList.html
grg2 <- read_tsv("./data/grg/grg_validated_deceased_supercentenarians_since_2008.tsv",
                 skip = 1,
                 col_names = c("name", "dob", "dod"),
                 col_types = "c--cc-------")

grg2 %>%
  mutate(
    src = "http://www.grg.org/SC/WorldSCRankingsList.html",
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
  ) -> grg2


bind_rows(grg1, grg2) %>%
  # specify x range
  filter(dod_y >= 1954 & dod_y < 2016) %>%
  # remove duplicates (the two grg sources have some overlap)
  filter(!(duplicated(name) & duplicated(dob) & duplicated(dod))) %>%
  group_by(dod_y) %>%
  # highest verified age at death by year
  filter(age_at_death_y == max(age_at_death_y)) %>%
  ungroup() %>%
  # the two regression groups
  mutate(group = ifelse(year(dod) < 1997, "a", "b")) -> grg_mrads

# Plot --------------------------------------------------------------------

# MRADs which are not in figure 6 by Dong etal.
not_in_dong <- anti_join(x = grg_mrads, y = dong_fig_6,
                         by = c("dod_dong_rounded" = "year", "age_at_death_dong_rounded" = "mrad"))

# plot reproduction against original
fig6_reproduce <-
  ggplot(grg_mrads, aes(x = dod_dong_rounded, y = age_at_death_dong_rounded)) +
  geom_point(aes(colour = group), size = 2.5, shape = 4) +
  geom_point(aes(x = year, y = mrad, colour = group),
             shape = 21, size = 2.5,
             data = dong_fig_6) +
  geom_text_repel(aes(label = name),
                  size = 2, colour = "#A7A7A7",
                  data = not_in_dong) +
  scale_y_continuous("MRAD from GRG",
                     breaks = seq(108, 124, 2),
                     limits = c(108,124),
                     expand = c(0,0)) +
  scale_x_continuous("Year",
                     breaks = seq(1950, 2020, 10),
                     limits = c(1950, 2020),
                     expand = c(0,0)) +
  scale_color_manual(values = c("#0061FF", "#FF4900"), guide = FALSE) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, colour = "grey"),
        panel.grid.minor = element_blank(),
        aspect.ratio = 0.8)

ggsave("./out/fig6_reproduce.pdf", fig6_reproduce,
       width = 6, height = 5, scale = 0.79)
