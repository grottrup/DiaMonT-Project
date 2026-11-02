library(tidyverse)

#################
### Load data ###
#################
data <- readRDS(file = "./GeneratedData/mabp_clean.rds")


##################################################
# Primary objective
##################################################

data <- data %>% 
  mutate(
    below70mmHg_cum = rowSums(70 > select(., starts_with("map_", ignore.case = FALSE)),na.rm = TRUE),
    below70mmHg = 2 < below70mmHg_cum,
    primary_outcome_good_functional_outcome = mrs_3m < 3
  )

##################################################
# Secondary objective
##################################################

data <- data %>% 
  mutate(
    secondary_outcome_alive = mrs_3m < 6
  )

data