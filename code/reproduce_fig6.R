#############################################
# Replicating Figure 6 of Dong etal. (2016) #
#############################################

# Init --------------------------------------------------------------------

library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

# Input -------------------------------------------------------------------

# grg data on supercentenarian deaths by year
sc_a <- read_excel("./data/grg_verified_supercentenarian_deaths_by_year_refuted_cases_removed.xlsx", sheet = 1)
sc_b <- read_excel("./data/grg_verified_supercentenarian_deaths_by_year_refuted_cases_removed.xlsx", sheet = 2)

# data taken from points on dong etal. figure 6
dong_fig_6 <- read_csv("./data/data_from_dong_etal_fig6.csv")

# merge both grg tables
bind_rows(sc_a, sc_b) %>%
  mutate(
    # specify date columns
    dob = mdy(dob), dod = mdy(dod),
    dob_y = year(dob), dob_m = month(dob), dob_d = day(dob),
    dod_y = year(dod), dod_m = month(dod), dod_d = day(dod),
    age_at_death_d = dod-dob,
    age_at_death_y = round((dod-dob)/365.2422, 3),
    # dong etal. round down to the next integer and add 0.2 years
    age_at_death_dong_rounded = floor(age_at_death_y)+0.2,
    # dong etal. center years mid-interval (of the preceeding interval)
    dod_dong_rounded = dod_y-0.5) -> sc


sc %>%
  # specify x range
  filter(dod_y >= 1954 & dod_y < 2016) %>%
  group_by(dod_y) %>%
  # highest verified age at death by year
  filter(age_at_death_d == max(age_at_death_d)) %>%
  ungroup() %>%
  # the two regression groups
  mutate(group = ifelse(year(dod) < 1997, "a", "b")) -> record_death

# Plot --------------------------------------------------------------------

# plot replication against original
fig6_compare <-
  ggplot(record_death, aes(x = dod_dong_rounded, y = age_at_death_dong_rounded)) +
  geom_point(aes(colour = group), size = 2.5, shape = 4) +
  geom_point(aes(x = year, y = mrad, colour = group),
             shape = 21, size = 2.5,
             data = dong_fig_6) +
  scale_y_continuous("MRAD from GRG",
                     breaks = seq(108, 124, 2),
                     limits = c(108,124),
                     expand = c(0,0)) +
  scale_x_continuous("Year",
                     breaks = seq(1950, 2020, 10),
                     limits = c(1950, 2020),
                     expand = c(0,0)) +
  scale_color_manual(values = c("#0061FF", "#FF4900"), guide = FALSE) +
  coord_fixed() +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, colour = "grey"),
        panel.grid.minor = element_blank(),
        aspect.ratio = 0.8)
fig6_compare
ggsave("./out/fig6_compare.pdf", fig6_compare, width = 6, height = 5, scale = 0.79)
