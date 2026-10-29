



library(lubridate)
library(dplyr)
library(ggplot2)
library(medisr)
library(VGAM)


data <- medisr::mabp
data <- tibble::tibble(data)



data <- data %>% 
  mutate(
    below70mmHg_cum = rowSums(70 > select(., starts_with("map_", ignore.case = FALSE)),na.rm = TRUE),
    below70mmHg = 2 < below70mmHg_cum,
    good_functional_outcome = mrs_3m < 3
  )










