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
  )


##################################################
# Secondary objective
##################################################

data <- data %>% 
  mutate(
    alive = mrs_3m < 6, #skal slettes
    secondary_outcome_alive = mrs_3m < 6,
    secondary_exposure_hypertension = Hypertension_ == TRUE
  )

# tabel med relativ risiko

######################################
#table(data$mrs_3m)

# 67 patienter døde
#data$sex
# View(data)


#count_true <- table(data$high_map_mmHg
#)
#count_true

#table(data$alive, data$mrs_3m) #du skal vælge hvad du vil

#table(data$high_map_mmHg)
#prop.table(table( data$alive,data$high_map_mmHg ),margin = 1)

#Rcode3.1.2.Riskratiowith95%CIandp-value
###################
library(epitools)
library(risks)
data$Outcome_dicho <-factor(data$mrs_3m < 6,
labels = c("Good outcome (alive)",
"Bad outcome (dead)"))
data$Hypertension_ <-factor(data$Hypertension_,
labels = c("Control",
"Hypertension"))

results <-epitab(table(data$below70mmHg_cum,data$Outcome_dicho),
method = "riskratio",
conf.level = 0.95,
pvalue = "fisher.exact")
prop.table(table(data$below70mmHg_cum,data$Outcome_dicho),margin=1)*100
results$tab

#
