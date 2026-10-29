###############
### Library ###
###############

library(lubridate)
library(dplyr)
library(ggplot2)
library(medisr)
library(VGAM)

#################
### Load data ###
#################
#medisr::medisr_init_project()
data <- medisr::mabp
data <- tibble::tibble(data)


?medisr::mabp
#dataclean
# sex, diabetes, hypertension og eventuelt NIHSS
# 1) Mange af variablene i datasættet er skrevet som numeriske værdier e.g. sex=0/1. Lav nogle nye character variable, e.g. sex_chr=Male/Female

data <- data |> 
  mutate(
    Sex_=ifelse(sex==1,yes="Female",no="Male"),
    Diabetes_=ifelse(Diabetes==1,yes="Diabetes",no="No diabetes"),
    Hypertension_=ifelse(Hyperton==1, yes="Hypertension", no="No hypertension"),
    stroke_severity = case_when(
      nihss_adm == 0 ~ "No stroke",
      nihss_adm >= 1 & nihss_adm <= 4 ~ "Minor stroke",
      nihss_adm >= 5 & nihss_adm <= 15 ~ "Moderate stroke",
      nihss_adm >= 16 & nihss_adm <= 20 ~ "Moderate to severe stroke",
      nihss_adm >= 21 & nihss_adm <= 42 ~ "Severe stroke",
      TRUE ~ NA_character_
    )
  )



data <- data %>% 
  mutate(
    below70mmHg_cum = rowSums(70 > select(., starts_with("map_", ignore.case = FALSE)),na.rm = TRUE),
    below70mmHg = 2 < below70mmHg_cum,
    primary_outcome_good_functional_outcome = mrs_3m < 3
  )

data
