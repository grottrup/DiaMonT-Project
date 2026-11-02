library(tidyverse)

#################
### Load data ###
#################
data <- readRDS(file = "./Case/GeneratedData/mabp_clean.rds")


##################################################
# Primary objective
##################################################
data <- data %>% 
  mutate(
    below70mmHg_cum = rowSums(70 > select(., starts_with("map_", ignore.case = FALSE)),na.rm = TRUE),
    above120mmHg_cum = rowSums(120 < select(., starts_with("map_", ignore.case = FALSE)),na.rm = TRUE),
    below70mmHg = 2 < below70mmHg_cum,
    above120mmHg = 2 < above120mmHg_cum,
    outs70120mmHg = 2 < above120mmHg_cum | 2 < below70mmHg_cum,
    good_functional_outcome = mrs_3m < 4,
    high_map_mmHg_cum = rowSums(100 < select(., starts_with("map_", ignore.case = FALSE)),na.rm = TRUE),
    high_map_mmHg = 5 > high_map_mmHg_cum,
    primary_outcome_good_functional_outcome = mrs_3m < 3,
    alive = mrs_3m < 6
  )


##################################################
# Secondary objective
##################################################

data <- data %>% 
  mutate(
    secondary_outcome_alive = mrs_3m < 6
  )

######################################
table(data$mrs_3m)

# 67 patienter døde
data$sex
# View(data)


count_true <- table(data$high_map_mmHg
)
count_true

table(data$alive, data$mrs_3m) #du skal vælge hvad du vil

table(data$high_map_mmHg)
prop.table(table( data$alive,data$high_map_mmHg ),margin = 1)



#exp(coef())
#exp(cibfubt())
