#######################################################
# Reproducing Plateau MRAD value of Dong etal. (2016) #
#######################################################

# Jonas Schöley, 2016-11-03

# Init --------------------------------------------------------------------

library(readr)
library(dplyr)

# Input -------------------------------------------------------------------

# verified supercentenarian deaths by country and year
# Source: "International Database on Longevity" (supercentenarians.org)
sapply(list.files(path = "./data/idl/"),
       function (x) read_delim(paste0("./data/idl/", x),
                               skip = 1,
                               col_names = c("id", "age_y", "age_d", "dob", "dod", "validation"),
                               col_types = "ii-i---ccc", delim = ";"),
       simplify = FALSE, USE.NAMES = TRUE) -> idl

# merge all data
bind_rows(idl, .id = "file") %>%
  # specify integer year dates
  mutate(dob = as.integer(gsub("^[[:digit:]]{2}/[[:digit:]]{2}/", "", dob)),
         dod = as.integer(gsub("^[[:digit:]]{2}/[[:digit:]]{2}/", "", dod))) %>%
  # add age at rational
  mutate(age_y_frac = age_d/365.2422) %>%
  # only include perfectly validated cases
  filter(validation == "A") %>%
  # find the maximum recorded age at death for each year
  group_by(dod) %>%
  filter(age_y_frac == max(age_y_frac)) %>%
  ungroup() %>%
  # group the data like dong etal.
  mutate(group = ifelse(dod < 1995, "a", "b")) -> max_age_at_death

# Reproduction ------------------------------------------------------------

# reproduce Dong etal. MRAD plateau value and CI
# and recalculate it based on exact age
max_age_at_death %>%
  filter(group == "b") %>%
  summarise(
    n = n(),
    integer_sd = sd(age_y),
    exact_sd = sd(age_y_frac),
    dongs_mean = mean(age_y),
    dongs_lower_95conf = dongs_mean-qt(0.975, n-1)*integer_sd/sqrt(n),
    dongs_upper_95conf = dongs_mean+qt(0.975, n-1)*integer_sd/sqrt(n),
    exact_mean = mean(age_y_frac),
    exact_lower_95conf = exact_mean-qt(0.975, n-1)*exact_sd/sqrt(n),
    exact_upper_95conf = exact_mean+qt(0.975, n-1)*exact_sd/sqrt(n)
  ) -> plateau